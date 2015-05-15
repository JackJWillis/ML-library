#############
### This file contains graphing functions for plotting the output from the ML_functions
#############

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

join_dfs <- function(dfs, keep_fold=FALSE) {
  if(length(dfs) != 1) {
    stopifnot(check_ids_match(dfs))
    stopifnot(check_trues_match(dfs))
  }
  if(is.null(names(dfs))) {
    names(dfs) <- seq_len(length(dfs))
  }
  df <- data.frame(matrix(nrow=nrow(dfs[[1]]), ncol=0))
  df$id <- dfs[[1]]$id
  df$true <- dfs[[1]]$true
  
  rename_columns <- function(name, df) {
    new_df <- data.frame(matrix(nrow=nrow(df), ncol=0))
    new_df[, name] <- df$predicted
    if(keep_fold) {
      new_df[, paste(name, "fold", sep="_")] <- df$fold
    }
    new_df[, "id"] <- df$id
    new_df
  }

  join_two <- function(df1, df2) {
    merge(df1, df2, by="id")
  }

  renamed_dfs <- mapply(rename_columns, names(dfs), dfs, SIMPLIFY=FALSE)
  Reduce(join_two, renamed_dfs, init=df)
}


plot_scatter <- function(...) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  melted <- reshape2::melt(joined, value.name="predicted", variable.name="method", id=c("id", "true"))
  ggplot2::ggplot(melted, ggplot2::aes(x=true, y=predicted, color=method)) +
    ggplot2::geom_point(alpha=0.5)
}

plot_density <- function(...) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  melted <- reshape2::melt(joined, variable.name="method", id=c("id"))
  ggplot2::ggplot(melted, ggplot2::aes(x=value, fill=method)) +
    ggplot2::geom_density(alpha=0.5)
}


#' Produce an ROC curve which plots a given method's sensitivity/specificity with respect
#' a given poverty threshold.
plot_roc <- function(THRESHOLD, ...) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  joined$response <- joined$true < THRESHOLD
  joined$id <- NULL
  joined$true <- NULL
  
  roc_formula <- as.formula(paste("response ~", paste(names(dfs), collapse="+")))
  rocs <- pROC::roc(roc_formula, data=joined, plot=FALSE)
  if(length(dfs) == 1) {
    rocs <- list(rocs)
    names(rocs) <- names(dfs)
  }
  roc_to_df <- function(name, roc) {
    data.frame(sensitivity=roc$sensitivities, specificity=roc$specificities, method=name)
  }
  roc_dfs <- mapply(roc_to_df, names(rocs), rocs, SIMPLIFY=FALSE)
  roc_df <- do.call("rbind", roc_dfs)
  ggplot2::ggplot(roc_df, ggplot2::aes(x=specificity, y=sensitivity, color=method)) +
    ggplot2::geom_step() +
    ggplot2::scale_x_reverse() +
    ggplot2::geom_abline(intercept=1, slope=1, alpha=0.5) 
  # TODO display auc
}


plot_cumulative <- function(joined, methods, fun, threshold, y_label, display_cutoffs, point_count) {
  ranked <- data.frame(mapply(fun, methods, list(joined), SIMPLIFY=FALSE))
  plot_points <- seq(1, nrow(joined), nrow(joined) / point_count)
  cut <- ranked[plot_points, , drop=FALSE]
  cut$percent_population_included <- plot_points / nrow(joined) 
  melted <- reshape2::melt(cut,
                           variable.name="method",
                           id="percent_population_included")
  p <- ggplot2::ggplot(melted, ggplot2::aes(x=percent_population_included, y=value, color=method)) +
    ggplot2::geom_step() +
    ggplot2::geom_point() +
    ggplot2::labs(y = y_label)

  if(display_cutoffs) {
    # TODO annotate plot
    threshold.df <- data.frame(
      idx=sapply(methods, function(name) {sum(joined[, name] < threshold)}),
      method=methods)
    threshold.df$percent_total <- threshold.df$idx / nrow(ranked)
    threshold.df$y <- mapply(function(idx, name) ranked[idx, name],  threshold.df[, "idx"], threshold.df[, "method"])
    p <- p +
      ggplot2::geom_segment(
        data=threshold.df,
        mapping=ggplot2::aes(y=y, x=percent_total, yend=y, xend=1., color=method))
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
  true_poor <- sum(joined$response)

  get_coverage <- function(method, df) {
    cumsum(df[order(df[, method]), "response"]) / true_poor
  }

  methods <- names(dfs)
  if(DISPLAY_TRUE) {
    methods <- c(methods, "true")
  }
  plot_cumulative(joined=joined,
                  methods=methods,
                  fun=get_coverage,
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

  get_to_true_poor <- function(method, df) {
    cumsum(df[order(df[, method]), "response"]) / seq(1, nrow(joined))
  }

  methods <- names(dfs)
  plot_cumulative(joined=joined,
                  methods=methods,
                  fun=get_to_true_poor,
                  threshold=THRESHOLD,
                  y_label="to_true_poor",
                  display_cutoffs=DISPLAY_CUTOFFS,
                  point_count=POINT_COUNT)
}
