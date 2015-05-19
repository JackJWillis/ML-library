#' @import dplyr

check_ids_match <- function(dfs) {
  ids <- lapply(dfs, function(df) df$id)
  all_equal <- lapply(ids, function(id_list) id_list == ids[[1]])
  all(unlist(all_equal))
}

check_trues_match <- function(dfs) {
  trues <- lapply(dfs, function(df) df$true)
  all_equal <- lapply(trues, function(trues_list) all.equal(trues_list, trues[[1]]))
  all(unlist(all_equal))
}

join_dfs <- function(dfs) {
  if(length(dfs) != 1) {
    stopifnot(check_ids_match(dfs))
    stopifnot(check_trues_match(dfs))
  }
  if(is.null(names(dfs))) {
    names(dfs) <- seq_len(length(dfs))
  }
  for( name in names(dfs)) {
    dfs[[name]]$method <- name
  }
  joined <- do.call("rbind", dfs)
  true_df <- data.frame(
    true=dfs[[1]]$true,
    predicted=dfs[[1]]$true,
    id=dfs[[1]]$id,
    method="true",
    fold=1)
  joined <- rbind(true_df, joined)
  joined$fold <- factor(joined$fold)
  joined
}


plot_scatter <- function(...) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  joined <- dplyr::filter(joined, method != "true")
  ggplot2::ggplot(joined, ggplot2::aes(x=true, y=predicted, color=method)) +
    ggplot2::geom_point(alpha=0.5)
}


plot_density <- function(..., SHOW_FOLDS=TRUE) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  p <- ggplot2::ggplot(joined, ggplot2::aes(x=predicted, color=method)) +
    ggplot2::geom_density()
  if (SHOW_FOLDS) {
    no_true <- filter(joined, method != "true")
    p <- p + ggplot2::geom_density(no_true, mapping=ggplot2::aes(x=predicted, color=method, group=fold), alpha=0.1)
  }
  p
}


#' Produce an ROC curve which plots a given method's sensitivity/specificity with respect
#' a given poverty threshold.
plot_roc <- function(THRESHOLD, ..., PLOT_FOLDS=FALSE) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  joined <- dplyr::filter(joined, method != "true")
  joined$response <- joined$true < THRESHOLD
  joined$id <- NULL
  joined$true <- NULL
  
  if (PLOT_FOLDS) {
    grouped <- dplyr::group_by(joined, method, fold)
  }
  else {
    grouped <- dplyr::group_by(joined, method)
  }
  
  rocs <- dplyr::do(grouped, roc=pROC::roc(response ~ predicted, data=., plot=FALSE))
  if (!PLOT_FOLDS) {
    rocs$fold <- 1
  }
  roc_to_df <- function(name, fold, roc) {
    data.frame(sensitivity=roc$sensitivities, specificity=roc$specificities, method=name, fold=fold)
  }
  roc_dfs <- mapply(roc_to_df, rocs$method, rocs$fold, rocs$roc, SIMPLIFY=FALSE)
  roc_df <- do.call("rbind", roc_dfs)
  
  
  if (PLOT_FOLDS) {
    aes <- ggplot2::aes(x=specificity, y=sensitivity, color=method, group=fold)
  }
  else {
    aes <- ggplot2::aes(x=specificity, y=sensitivity, color=method)
  }
  ggplot2::ggplot(roc_df, aes) +
    ggplot2::geom_step(alpha=0.5) +
    ggplot2::scale_x_reverse() +
    ggplot2::geom_abline(intercept=1, slope=1, alpha=0.5) 
  # TODO display auc
}


plot_cumulative <- function(df, threshold, y_label, display_cutoffs, point_count) {
  cut <- df %>%
    slice(seq(1, n(), n() / point_count))
    
  p <- ggplot2::ggplot(cut, ggplot2::aes(x=percent_population_included, y=value, color=method)) +
    ggplot2::geom_step() +
    ggplot2::geom_point() +
    ggplot2::labs(y = y_label)

  if(display_cutoffs) {
    # TODO annotate plot
    threshold.df <- df %>%
      filter(cumall(predicted < threshold)) %>%
      filter(row_number() == n())
    p <- p +
      ggplot2::geom_segment(
        data=threshold.df,
        mapping=ggplot2::aes(y=value, x=percent_population_included, yend=value, xend=1., color=method))
  }
  p
}


#' If we target N people, what fraction of the true poor would receive funds?
#' True Positives / Total Positives
#' Note that this is a reparameterization of the ROC curve
plot_accuracy <- function(THRESHOLD, ..., DISPLAY_TRUE=FALSE, DISPLAY_CUTOFFS=FALSE, POINT_COUNT=20) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  joined$response <- joined$true < THRESHOLD
  if(!DISPLAY_TRUE) {
    joined <- dplyr::filter(joined, method!="true")
  }

  df <- joined %>%
    group_by(method) %>%
    arrange(predicted) %>%
    mutate(value=cumsum(response) / sum(response)) %>%
    mutate(percent_population_included=row_number() / n())

  plot_cumulative(df=df,
                  threshold=THRESHOLD,
                  y_label="coverage",
                  display_cutoffs=DISPLAY_CUTOFFS,
                  point_count=POINT_COUNT)
}

#' With a fixed amount of money, if we target N people, what fraction would go to the true poor?
#' True Positives / (True Positives + False Positives)
plot_accuracy_dollars <- function(THRESHOLD, ..., DISPLAY_TRUE=FALSE, DISPLAY_CUTOFFS=FALSE, POINT_COUNT=20) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  joined$response <- joined$true < THRESHOLD
  if (!DISPLAY_TRUE) {
      joined <- dplyr::filter(joined, method!="true")
  }

  df <- joined %>%
    group_by(method) %>%
    arrange(predicted) %>%
    mutate(value=cumsum(response) / row_number()) %>%
    mutate(percent_population_included=row_number() / n())

  plot_cumulative(df=df,
                  threshold=THRESHOLD,
                  y_label="to_true_poor",
                  display_cutoffs=DISPLAY_CUTOFFS,
                  point_count=POINT_COUNT)
}


plot_swf <- function(..., GAMMA=10, POINT_COUNT=20) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  joined <- dplyr::filter(joined, method!="true")
  marginal_utility <- function(consumption) consumption ^ (- GAMMA)
  joined$marginal_utility <- sapply(joined$true, marginal_utility)

  df <- joined %>%
    group_by(method) %>%
    arrange(predicted) %>%
    mutate(value=cumsum(marginal_utility) / cumsum(sort(marginal_utility, decreasing=TRUE)))%>%
    mutate(percent_population_included=row_number() / n())

  plot_cumulative(df=df,
                  threshold=NULL,
                  y_label="welfare",
                  display_cutoffs=FALSE,
                  point_count=POINT_COUNT)
}
