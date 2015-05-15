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

plot_density<- function(...) {
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


#' If we target N people, what fraction of the true poor would receive funds?
#' True Positives / Total Positives
plot_accuracy <- function(THRESHOLD, ..., POINT_COUNT=20) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  N <- nrow(joined)
  plot_points <- seq(1, N, length=POINT_COUNT)
  joined$response <- joined$true < THRESHOLD
  true_poor <- sum(joined$response)

  get_coverage <- function(method, df) {
    cumsum(df[order(df[, method]), "response"]) / true_poor
  }
  ranked <- data.frame(mapply(get_coverage, names(dfs), list(joined), SIMPLIFY=FALSE))
  
  cut <- ranked[plot_points, ,drop=FALSE]
  cut$percent_population_included <- plot_points / N
  melted <- reshape2::melt(
    cut, 
    value.name="coverage",
    variable.name="method",
    id="percent_population_included")
  ggplot2::ggplot(melted, ggplot2::aes(x=percent_population_included, y=coverage, color=method)) +
    ggplot2::geom_step() +
    ggplot2::geom_point()
}


#' With a fixed amount of money, if we target N people, what fraction would go to the true poor?
#' True Positives / (True Positives + False Positives)
plot_accuracy_dollars <- function(THRESHOLD, ..., POINT_COUNT=20) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  N <- nrow(joined)
  plot_points <- seq(1, N, length=POINT_COUNT)
  joined$response <- joined$true < THRESHOLD

  get_to_true_poor <- function(method, df) {
    cumsum(df[order(df[, method]), "response"]) / seq(1, N)
  }
  ranked <- data.frame(mapply(get_to_true_poor, names(dfs), list(joined), SIMPLIFY=FALSE))
  
  cut <- ranked[plot_points, , drop=FALSE]
  cut$percent_population_included <- plot_points / N
  melted <- reshape2::melt(
    cut, 
    value.name="to_true_poor",
    variable.name="method",
    id="percent_population_included")
  ggplot2::ggplot(melted, ggplot2::aes(x=percent_population_included, y=to_true_poor, color=method)) +
    ggplot2::geom_step() +
    ggplot2::geom_point()
}
