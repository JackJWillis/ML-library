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
  # if(length(dfs) != 1) {
  #   stopifnot(check_ids_match(dfs))
  #   stopifnot(check_trues_match(dfs))
  # }
  if(is.null(names(dfs))) {
    names(dfs) <- seq_len(length(dfs))
  }
  for( name in names(dfs)) {
    dfs[[name]]$method <- name
    dfs[[name]]$true <- as.numeric(dfs[[name]]$true)
  }
  joined <- do.call("rbind", dfs)
  true_df <- data.frame(
    true=dfs[[1]]$true,
    predicted=dfs[[1]]$raw,
    raw=dfs[[1]]$raw,
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


plot_density <- function(..., SHOW_FOLDS=FALSE) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  p <- ggplot2::ggplot(joined, ggplot2::aes(x=predicted, color=method)) +
    ggplot2::geom_density()
  if (SHOW_FOLDS) {
    no_true <- dplyr::filter(joined, method != "true")
    p <- p + ggplot2::geom_density(no_true, mapping=ggplot2::aes(x=predicted, color=method, group=fold), alpha=0.1)
  }
  p
}


#' Produce an ROC curve which plots a given method's sensitivity/specificity with respect
#' a given poverty threshold.
plot_roc <- function(THRESHOLD, ..., SHOW_FOLDS=FALSE) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  joined <- dplyr::filter(joined, method != "true")
  joined$response <- joined$raw < THRESHOLD
  joined$id <- NULL
  joined$true <- NULL
  
  roc_to_df <- function(name, fold, roc) {
    data.frame(sensitivity=roc$sensitivities, specificity=roc$specificities, method=name, fold=fold)
  }
  
  # Generate per method roc curves
  grouped <- dplyr::group_by(joined, method)
  rocs <- dplyr::do(grouped, roc=pROC::roc(response ~ predicted, data=., plot=FALSE))
  roc_dfs <- mapply(roc_to_df, rocs$method, 1, rocs$roc, SIMPLIFY=FALSE)
  roc_df <- do.call("rbind", roc_dfs)
  aes <- ggplot2::aes(x=specificity, y=sensitivity, color=method)
  p <- ggplot2::ggplot(roc_df, aes) +
    ggplot2::geom_step(alpha=0.8) +
    ggplot2::scale_x_reverse() +
    ggplot2::geom_abline(intercept=1, slope=1, alpha=0.5) 
  # TODO show auc
  
  if (SHOW_FOLDS) {
    # Generate per method-fold roc curved
    grouped <- dplyr::group_by(joined, method, fold)
    rocs <- dplyr::do(grouped, roc=pROC::roc(response ~ predicted, data=., plot=FALSE))
    roc_dfs <- mapply(roc_to_df, rocs$method, rocs$fold, rocs$roc, SIMPLIFY=FALSE)
    roc_df <- do.call("rbind", roc_dfs)
    aes <- ggplot2::aes(x=specificity, y=sensitivity, color=method, group=fold)
    p <- p + ggplot2::geom_step(data=roc_df, mapping=aes, alpha=0.3)
  }
  p
}


plot_cumulative <- function(df, threshold, y_label, show_cutoffs, show_folds, folded, point_count) {
  cut <- df %>%
    slice(seq(1, n(), n() / point_count))
    
  p <- ggplot2::ggplot(cut, ggplot2::aes(x=percent_population_included, y=value, color=method)) +
    ggplot2::geom_step() +
    ggplot2::geom_point() +
    ggplot2::labs(y = y_label)

  if (show_cutoffs) {
    # TODO annotate plot
    threshold.df <- df %>%
      filter(cumall(predicted < threshold)) %>%
      filter(row_number() == n())
    p <- p +
      ggplot2::geom_segment(
        data=threshold.df,
        mapping=ggplot2::aes(y=value, x=percent_population_included, yend=value, xend=1., color=method))
  }
  if (show_folds) {
    folded_cut <- folded %>% slice(seq(1, n(), n() / point_count))
    aes <- ggplot2::aes(x=percent_population_included, y=value, color=method, group=fold)
    p <- p + ggplot2::geom_step(data=folded_cut, mapping=aes, alpha=0.3)
  }
  p
}


#' If we target N people, what fraction of the true poor would receive funds?
#' True Positives / Total Positives
#' Note that this is a reparameterization of the ROC curve
plot_accuracy <- function(THRESHOLD, ..., SHOW_TRUE=FALSE, SHOW_CUTOFFS=FALSE, SHOW_FOLDS=FALSE, POINT_COUNT=20) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  joined$response <- joined$raw < THRESHOLD
  if(!SHOW_TRUE) {
    joined <- dplyr::filter(joined, method!="true")
  }
  make_df <- function(df, folds) {

    if (folds) {
      grouped <- group_by(df, method, fold)
    }
    else {
      grouped <- group_by(df, method)
    }
    grouped %>%
      arrange(predicted) %>%
      mutate(value=cumsum(response) / sum(response)) %>%
      mutate(percent_population_included=row_number() / n())
  }

  df <- make_df(joined, FALSE)
  folded_df <- make_df(joined, TRUE)
  plot_cumulative(df=df,
                  threshold=THRESHOLD,
                  y_label="coverage",
                  show_cutoffs=SHOW_CUTOFFS,
                  show_folds=SHOW_FOLDS,
                  folded=folded_df,
                  point_count=POINT_COUNT)
}

#' With a fixed amount of money, if we target N people, what fraction would go to the true poor?
#' True Positives / (True Positives + False Positives)
plot_accuracy_dollars <- function(THRESHOLD, ..., SHOW_TRUE=FALSE, SHOW_CUTOFFS=FALSE, SHOW_FOLDS=FALSE, POINT_COUNT=20) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  joined$response <- joined$raw < THRESHOLD
  if (!SHOW_TRUE) {
      joined <- dplyr::filter(joined, method!="true")
  }

  make_df <- function(df, folds) {

    if (folds) {
      grouped <- group_by(df, method, fold)
    }
    else {
      grouped <- group_by(df, method)
    }
    grouped %>%
      arrange(predicted) %>%
      mutate(value=cumsum(response) / row_number()) %>%
      mutate(percent_population_included=row_number() / n())
  }

  df <- make_df(joined, FALSE)
  folded_df <- make_df(joined, TRUE)
  plot_cumulative(df=df,
                  threshold=THRESHOLD,
                  y_label="to_true_poor",
                  show_cutoffs=SHOW_CUTOFFS,
                  show_folds=SHOW_FOLDS,
                  folded=folded_df,
                  point_count=POINT_COUNT)
}


plot_swf <- function(..., GAMMA=10, SHOW_FOLDS=FALSE, POINT_COUNT=20) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  joined <- dplyr::filter(joined, method!="true")
  marginal_utility <- function(log_consumption) exp(log_consumption) ^ (- GAMMA)
  joined$marginal_utility <- sapply(joined$raw, marginal_utility)

  make_df <- function(df, folds) {

    if (folds) {
      grouped <- group_by(df, method, fold)
    }
    else {
      grouped <- group_by(df, method)
    }
    grouped %>%
      arrange(predicted) %>%
      mutate(value=cumsum(marginal_utility) / cumsum(sort(marginal_utility, decreasing=TRUE)))%>%
      mutate(percent_population_included=row_number() / n())
  }

  df <- make_df(joined, FALSE)
  folded_df <- make_df(joined, TRUE)
  plot_cumulative(df=df,
                  threshold=NULL,
                  y_label="welfare",
                  show_cutoffs=FALSE,
                  show_folds=SHOW_FOLDS,
                  folded=folded_df,
                  point_count=POINT_COUNT)
}
