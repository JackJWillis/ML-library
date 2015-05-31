#' @import dplyr
#' @export

DEFAULT_PERCENTILES <- seq(0.1, 0.9, by=0.1)

join_dfs <- function(dfs) {
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
    method="true",
    fold=1)
  joined <- rbind(true_df, joined)
  joined$fold <- factor(joined$fold)
  joined
}


plot_residuals_ <- function(joined) {
  joined <- dplyr::filter(joined, method != "true")
  joined$residual <- joined$true - joined$predicted
  ggplot2::ggplot(joined, ggplot2::aes(x=predicted, y=residual, color=method)) +
    ggplot2::geom_point(alpha=0.5)
}

plot_residuals <- function(...) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  plot_residuals_(joined)
}


plot_scatter_ <- function(joined) {
  joined <- dplyr::filter(joined, method != "true")
  ggplot2::ggplot(joined, ggplot2::aes(x=true, y=predicted, color=method)) +
    ggplot2::geom_point(alpha=0.5)
}


plot_scatter <- function(...) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  plot_scatter_(joined)
}


plot_density_ <- function(joined, SHOW_FOLDS=FALSE) {
  p <- ggplot2::ggplot(joined, ggplot2::aes(x=predicted, color=method)) +
    ggplot2::geom_density()
  if (SHOW_FOLDS) {
    no_true <- dplyr::filter(joined, method != "true")
    p <- p + ggplot2::geom_density(no_true, mapping=ggplot2::aes(x=predicted, color=method, group=fold), alpha=0.1)
  }
  p
}

plot_density <- function(..., SHOW_FOLDS=FALSE) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  plot_density_(joined, SHOW_FOLDS)
}


plot_roc_ <- function(joined, THRESHOLD=DEFAULT_THRESHOLDS, SHOW_FOLDS=FALSE) {
  joined <- dplyr::filter(joined, method != "true")
  joined <- joined[rep(seq_len(nrow(joined)), each=length(THRESHOLD)), ]
  joined$threshold <- THRESHOLD
  joined$true <- NULL
  
  roc_to_df <- function(name, fold, roc, threshold) {
    data.frame(sensitivity=roc$sensitivities, specificity=roc$specificities, method=name, fold=fold, threshold=threshold)
  }
  
  # Generate per method roc curves
  grouped <- dplyr::group_by(joined, method, threshold) %>%
    mutate(response=raw < quantile(raw, threshold))
  rocs <- dplyr::do(grouped, roc=pROC::roc(response ~ predicted, data=., plot=FALSE))
  roc_dfs <- mapply(roc_to_df, rocs$method, 1, rocs$roc, rocs$threshold, SIMPLIFY=FALSE)
  roc_df <- do.call("rbind", roc_dfs)
  aes <- ggplot2::aes(x=specificity, y=sensitivity, color=method)
  p <- ggplot2::ggplot(roc_df, aes) +
    ggplot2::geom_step(alpha=0.8) +
    ggplot2::scale_x_reverse() +
    ggplot2::geom_abline(intercept=1, slope=1, alpha=0.5) +
    ggplot2::facet_wrap(~ threshold)
  # TODO show auc
  
  if (SHOW_FOLDS) {
    # Generate per method-fold roc curved
    grouped <- dplyr::group_by(joined, method, fold, threshold) %>%
      mutate(response=raw < quantile(raw, threshold))
    rocs <- dplyr::do(grouped, roc=pROC::roc(response ~ predicted, data=., plot=FALSE))
    roc_dfs <- mapply(roc_to_df, rocs$method, rocs$fold, rocs$roc, rocs$threshold, SIMPLIFY=FALSE)
    roc_df <- do.call("rbind", roc_dfs)
    aes <- ggplot2::aes(x=specificity, y=sensitivity, color=method, group=fold)
    p <- p + ggplot2::geom_step(data=roc_df, mapping=aes, alpha=0.3)
  }
  p
}

#' Produce an ROC curve which plots a given method's sensitivity/specificity with respect
#' a given poverty threshold.
plot_roc <- function(..., THRESHOLD=DEFAULT_THRESHOLDS, SHOW_FOLDS=FALSE) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  plot_roc_(joined, THRESHOLD, SHOW_FOLDS)
}


plot_cumulative <- function(df, y_label, show_cutoffs, show_folds, folded, point_count) {
  cut <- df %>%
    slice(seq(1, n(), n() / point_count))
    
  p <- ggplot2::ggplot(cut, ggplot2::aes(x=percent_population_included, y=value, color=method)) +
    ggplot2::geom_step() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ threshold) +
    ggplot2::labs(y = y_label)

  if (show_cutoffs) {
    # TODO annotate plot
    df <- mutate(df, absolute=quantile(raw, threshold))
    threshold.df <- df %>%
      filter(cumall(predicted < absolute)) %>%
      filter(row_number() == n())
    threshold.df[["cutoff"]] <- "consumption"
    
    percentile.threshold.df <- df %>%
      mutate(percentile_count = sum(raw < absolute)) %>%
      filter(percentile_count == row_number())
    percentile.threshold.df[["cutoff"]] <- "percentile"
    percentile.threshold.df$percentile_count <- NULL
    
    median.threshold.df <- df %>%
      filter(cumall(predicted < median(raw))) %>%
      filter(row_number() == n())
    median.threshold.df[["cutoff"]] <- "median"
      
    threshold.df <- rbind(threshold.df, percentile.threshold.df, median.threshold.df)
    
    horizontal_mapping <- ggplot2::aes(y=value, x=0., yend=value, xend=percent_population_included, color=method, linetype=cutoff)
    vertical_mapping <- ggplot2::aes(y=0., x=percent_population_included, yend=value, xend=percent_population_included, color=method, linetype=cutoff)
    p <- p +
      ggplot2::geom_segment(data=threshold.df, mapping=horizontal_mapping) +
      ggplot2::geom_segment(data=threshold.df, mapping=vertical_mapping)
  }
  if (show_folds) {
    folded_cut <- folded %>% slice(seq(1, n(), n() / point_count))
    aes <- ggplot2::aes(x=percent_population_included, y=value, color=method, group=fold)
    p <- p + ggplot2::geom_step(data=folded_cut, mapping=aes, alpha=0.3)
  }
  p
}


plot_accuracy_ <- function(joined, THRESHOLD=DEFAULT_THRESHOLDS, SHOW_TRUE=FALSE, SHOW_CUTOFFS=FALSE, SHOW_FOLDS=FALSE, POINT_COUNT=20) {
  joined <- joined[rep(seq_len(nrow(joined)), each=length(THRESHOLD)), ]
  joined$threshold <- THRESHOLD
  if(!SHOW_TRUE) {
    joined <- dplyr::filter(joined, method!="true")
  }
  make_df <- function(df, folds) {

    if (folds) {
      grouped <- group_by(df, method, fold, threshold)
    }
    else {
      grouped <- group_by(df, method, threshold)
    }
    grouped %>%
      mutate(response=raw < quantile(raw, threshold)) %>%
      arrange(predicted) %>%
      mutate(value=cumsum(response) / sum(response)) %>%
      mutate(percent_population_included=row_number() / n())
  }

  df <- make_df(joined, FALSE)
  folded_df <- make_df(joined, TRUE)
  plot_cumulative(df=df,
                  y_label="coverage",
                  show_cutoffs=SHOW_CUTOFFS,
                  show_folds=SHOW_FOLDS,
                  folded=folded_df,
                  point_count=POINT_COUNT)
}


#' If we target N people, what fraction of the true poor would receive funds?
#' True Positives / Total Positives
#' Note that this is a reparameterization of the ROC curve
plot_accuracy <- function(..., THRESHOLD=DEFAULT_THRESHOLDS, SHOW_TRUE=FALSE, SHOW_CUTOFFS=FALSE, SHOW_FOLDS=FALSE, POINT_COUNT=20) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  plot_accuracy_(joined, THRESHOLD, SHOW_TRUE, SHOW_CUTOFFS, SHOW_FOLDS, POINT_COUNT)
}


plot_accuracy_dollars_ <- function(joined, THRESHOLD=DEFAULT_THRESHOLDS, SHOW_TRUE=FALSE, SHOW_CUTOFFS=FALSE, SHOW_FOLDS=FALSE, POINT_COUNT=20) {
  joined <- joined[rep(seq_len(nrow(joined)), each=length(THRESHOLD)), ]
  joined$threshold <- THRESHOLD
  if (!SHOW_TRUE) {
      joined <- dplyr::filter(joined, method!="true")
  }

  make_df <- function(df, folds) {

    if (folds) {
      grouped <- group_by(df, method, fold, threshold)
    }
    else {
      grouped <- group_by(df, method, threshold)
    }
    grouped %>%
      mutate(response=raw < quantile(raw, threshold)) %>%
      arrange(predicted) %>%
      mutate(value=cumsum(response) / row_number()) %>%
      mutate(percent_population_included=row_number() / n())
  }

  df <- make_df(joined, FALSE)
  folded_df <- make_df(joined, TRUE)
  plot_cumulative(df=df,
                  y_label="to_true_poor",
                  show_cutoffs=SHOW_CUTOFFS,
                  show_folds=SHOW_FOLDS,
                  folded=folded_df,
                  point_count=POINT_COUNT)

}

#' With a fixed amount of money, if we target N people, what fraction would go to the true poor?
#' True Positives / (True Positives + False Positives)
plot_accuracy_dollars <- function(..., THRESHOLD=DEFAULT_THRESHOLDS, SHOW_TRUE=FALSE, SHOW_CUTOFFS=FALSE, SHOW_FOLDS=FALSE, POINT_COUNT=20) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  plot_accuracy_dollars_(joined, THRESHOLD, SHOW_TRUE, SHOW_CUTOFFS, SHOW_FOLDS, POINT_COUNT)
}


plot_swf_ <- function(joined, GAMMA=2, SHOW_FOLDS=FALSE, POINT_COUNT=20) {
  joined <- dplyr::filter(joined, method!="true")
  joined$threshold <- ""
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
                  y_label="welfare",
                  show_cutoffs=FALSE,
                  show_folds=SHOW_FOLDS,
                  folded=folded_df,
                  point_count=POINT_COUNT)
}


plot_swf <- function(..., GAMMA=2, SHOW_FOLDS=FALSE, POINT_COUNT=20) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  plot_swf_(joined, GAMMA, SHOW_FOLDS, POINT_COUNT)
}

plot_reach_vs_waste_ <- function(joined, THRESHOLD=DEFAULT_THRESHOLDS, SHOW_FOLDS=FALSE, POINT_COUNT=20) {
  joined <- joined[rep(seq_len(nrow(joined)), each=length(THRESHOLD)), ]
  joined$threshold <- THRESHOLD
  if(!SHOW_TRUE) {
    joined <- dplyr::filter(joined, method!="true")
  }
  make_df <- function(df, folds) {
    
    if (folds) {
      grouped <- group_by(df, method, fold, threshold)
    }
    else {
      grouped <- group_by(df, method, threshold)
    }
    grouped %>%
      mutate(response1=raw < quantile(raw, threshold)) %>%
      mutate(response2=raw >= quantile(raw, threshold)) %>%  
      arrange(predicted) %>%
      mutate(value=cumsum(response2) / n()) %>%
      #Note this is percent poor included, might wish to change later
      mutate(percent_population_included=cumsum(response1) / n())
  }
  
  df <- make_df(joined, FALSE)
  folded_df <- make_df(joined, TRUE)
  plot_cumulative(df=df,
                  y_label="number of rich targeted",
                  show_cutoffs=SHOW_CUTOFFS,
                  show_folds=SHOW_FOLDS,
                  folded=folded_df,
                  point_count=POINT_COUNT)
}

plot_reach_vs_waste <- function(..., THRESHOLD=DEFAULT_THRESHOLDS, SHOW_FOLDS=FALSE, POINT_COUNT=20) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  plot_reach_vs_waste_(joined, THRESHOLD, SHOW_FOLDS, POINT_COUNT)
}