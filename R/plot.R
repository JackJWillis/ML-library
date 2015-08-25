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
  true_df <- dfs[[1]]
  true_df$predicted <- true_df$raw
  true_df$method <- "true"
  true_df$fold <- 1
  joined <- rbind(true_df, joined)
  joined$fold <- factor(joined$fold)
  joined
}

add_dfs <- function(joined, dfs) {
  stopifnot(!is.null(names(dfs)))
  for( name in names(dfs)) {
    stopifnot(!(name %in% joined$method))
    dfs[[name]]$method <- name
    dfs[[name]]$true <- as.numeric(dfs[[name]]$true)
  }
  new_joined <- do.call("rbind", dfs)
  rbind(joined, new_joined)
  }

calculate_mse_ <- function(joined) {
  joined <- dplyr::filter(joined, method != "true")
  with_mse <- mutate(joined, se=(true - predicted)**2)
  grouped <- group_by(with_mse, method)
  summarise(grouped, mse=mean(se))
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

subtract_base <- function(df, base) {
  df <- mutate(df, i=row_number())
  base_df <- filter(df, method == base) %>% 
    ungroup() %>%
    group_by(i) %>%
    summarise(y=mean(y)) %>%
    select(one_of("y", "i")) %>%
    rename(base_y=y)
  df <- merge(df, base_df, on=i)
  df <- mutate(df, y=y-base_y)
  df <- filter(df, method != base)
  df
}

plot_cumulative <- function(df, y_label, show_cutoffs, show_folds, folded, point_count, x_label="percent_population_included", base=NULL) {
  if (!is.null(base)) {
    # assumes that methods are all the same size so check if they are not
    method_counts <- summarise(df, count=n())$count
    stopifnot(first(method_counts) == method_counts)
    df <- subtract_base(df, base)
    folded <- subtract_base(folded, base)
  }

  cut <- df %>%
    slice(seq(1, n(), n() / point_count))

  p <- ggplot2::ggplot(cut, ggplot2::aes(x=x, y=y, color=method)) +
    ggplot2::geom_line(alpha=0.75) +
    ggplot2::facet_wrap(~ threshold) +
    ggplot2::labs(y = y_label) +
    ggplot2::labs(x = x_label)
   
  if ('least_squares' %in% cut$method) {
    lms <- filter(cut, method=='least_squares')
    p <- p + 
      ggplot2::geom_line(data=lms, mapping=ggplot2::aes(x=x, y=y, color=method), size=1.75)
  }
  
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
    
    horizontal_mapping <- ggplot2::aes(y=y, x=0., yend=y, xend=x, color=method, linetype=cutoff)
    vertical_mapping <- ggplot2::aes(y=0., x=x, yend=y, xend=x, color=method, linetype=cutoff)
    p <- p +
      ggplot2::geom_segment(data=threshold.df, mapping=horizontal_mapping) +
      ggplot2::geom_segment(data=threshold.df, mapping=vertical_mapping)
  }
  if (show_folds) {
    # assumes that folds are all the same size so check if they are not
    fold_counts <- summarise(folded, count=n())
    method_fold_stats <- summarise(fold_counts, max=max(count), min=min(count))
    one_size <- method_fold_stats$max == method_fold_stats$min
    stopifnot(all(one_size))
    folded <- folded %>%
      group_by(method, fold) %>%
      mutate(i=row_number()) %>%
      ungroup() %>%
      group_by(method, i) %>%
      summarise(ymax=max(y), ymin=min(y), x=first(x))
    folded_cut <- folded %>% slice(seq(1, n(), n() / point_count))
    aes_min <- ggplot2::aes(x=x, y=ymin, color=method)
    aes_max <- ggplot2::aes(x=x, y=ymax, color=method)
    p <- p + ggplot2::geom_line(data=folded_cut, mapping=aes_min, alpha=0.5, linetype="dashed")
    p <- p + ggplot2::geom_line(data=folded_cut, mapping=aes_max, alpha=0.5, linetype="dashed")
  }
  p
}


plot_accuracy_ <- function(joined, BASE=NULL, THRESHOLD=DEFAULT_THRESHOLDS, SHOW_TRUE=FALSE, SHOW_CUTOFFS=FALSE, SHOW_FOLDS=FALSE, POINT_COUNT=200) {
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
      mutate(y=cumsum(response) / n()) %>%
      mutate(x=row_number() / n())
  }

  df <- make_df(joined, FALSE)
  folded_df <- make_df(joined, TRUE)
  plot_cumulative(df=df,
                  base=BASE,
                  y_label="coverage",
                  show_cutoffs=SHOW_CUTOFFS,
                  show_folds=SHOW_FOLDS,
                  folded=folded_df,
                  point_count=POINT_COUNT)
}


#' If we target N people, what fraction of the true poor would receive funds?
#' True Positives / Total Positives
#' Note that this is a reparameterization of the ROC curve
plot_accuracy <- function(..., BASE=NULL, THRESHOLD=DEFAULT_THRESHOLDS, SHOW_TRUE=FALSE, SHOW_CUTOFFS=FALSE, SHOW_FOLDS=FALSE, POINT_COUNT=200) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  plot_accuracy_(joined, BASE, THRESHOLD, SHOW_TRUE, SHOW_CUTOFFS, SHOW_FOLDS, POINT_COUNT)
}


plot_accuracy_dollars_ <- function(joined, BASE=NULL, THRESHOLD=DEFAULT_THRESHOLDS, SHOW_TRUE=FALSE, SHOW_CUTOFFS=FALSE, SHOW_FOLDS=FALSE, POINT_COUNT=200) {
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
      mutate(y=cumsum(response) / row_number()) %>%
      mutate(x=row_number() / n())
  }

  df <- make_df(joined, FALSE)
  folded_df <- make_df(joined, TRUE)
  plot_cumulative(df=df,
                  base=BASE,
                  y_label="to_true_poor",
                  show_cutoffs=SHOW_CUTOFFS,
                  show_folds=SHOW_FOLDS,
                  folded=folded_df,
                  point_count=POINT_COUNT)

}

#' With a fixed amount of money, if we target N people, what fraction would go to the true poor?
#' True Positives / (True Positives + False Positives)
plot_accuracy_dollars <- function(..., THRESHOLD=DEFAULT_THRESHOLDS, SHOW_TRUE=FALSE, SHOW_CUTOFFS=FALSE, SHOW_FOLDS=FALSE, POINT_COUNT=200) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  plot_accuracy_dollars_(joined, THRESHOLD, SHOW_TRUE, SHOW_CUTOFFS, SHOW_FOLDS, POINT_COUNT)
}


plot_swf_ <- function(joined, BASE=NULL, GAMMA=2, SHOW_FOLDS=FALSE, POINT_COUNT=200) {
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
      mutate(y=cumsum(marginal_utility) / cumsum(sort(marginal_utility, decreasing=TRUE)))%>%
      mutate(x=row_number() / n())
  }

  df <- make_df(joined, FALSE)
  folded_df <- make_df(joined, TRUE)
  plot_cumulative(df=df,
                  base=BASE,
                  y_label="welfare",
                  show_cutoffs=FALSE,
                  show_folds=SHOW_FOLDS,
                  folded=folded_df,
                  point_count=POINT_COUNT)
}


plot_swf <- function(..., BASE=NULL, GAMMA=2, SHOW_FOLDS=FALSE, POINT_COUNT=200) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  plot_swf_(joined, BASE, GAMMA, SHOW_FOLDS, POINT_COUNT)
}

calculate_reach_vs_waste_ <- function(df, folds) {
  if (folds){
    grouped <- group_by(df, method, fold, threshold)
  }
  else {
    grouped <- group_by(df, method, threshold)
  }
  wtd.quantile <- Hmisc::wtd.quantile
  grouped %>%
    mutate(response1=weight*as.numeric(raw < wtd.quantile(raw, weights=weight, probs=threshold))) %>%
    mutate(response2=weight*as.numeric(raw >= wtd.quantile(raw, weights=weight, probs=threshold))) %>%  
    arrange(predicted) %>%
    mutate(y=cumsum(response1) / sum(weight)) %>%
    mutate(x=cumsum(response2) / sum(weight)) %>%
    mutate(percent_pop_included=cumsum(weight) / sum(weight))
}

calculate_reach_ <- function(joined, fold=FALSE, poverty_threshold=.4, target_threshold=.4, base=NULL) {
  joined <- mutate(joined, threshold=poverty_threshold)
  rvw <- calculate_reach_vs_waste_(joined, folds=fold)
  folds <- unique(rvw$fold) 
  true <- filter(rvw, method == 'true')
  true <- true[rep(1:nrow(true), times=length(folds)), ]
  true$fold <- folds
  rvw <- filter(rvw, method != 'true')
  rvw <- rbind(rvw, true)
  reach_df <- rvw %>%
    filter(percent_pop_included < target_threshold) %>%
    mutate(reach=y) %>%
    arrange(desc(percent_pop_included))
  if (!is.null(base)) {
    base_reach <- reach_df %>%
      filter(method == base) %>%
      group_by(fold) %>%
      summarise(reach=first(reach))
    reach_df <- merge(reach_df, base_reach, by='fold') %>%
      mutate(reach=reach.x - reach.y) %>%
      filter(method != base) %>%
      group_by(method, fold)
  }
  reach_df <- reach_df %>%
    arrange(desc(percent_pop_included)) %>%
    summarise(reach=first(reach))
  select(reach_df, -one_of('threshold'))
}

calculate_budget_to_true_poor_ <- function(joined, fold=FALSE, poverty_threshold=.4, target_threshold=.4, base=NULL) {
  joined <- mutate(joined, threshold=poverty_threshold)
  if (fold){
    grouped <- group_by(joined, method, fold, threshold)
  }
  else {
    grouped <- group_by(joined, method, threshold)
  }
  bttp <- grouped %>%
    mutate(response=raw < quantile(raw, threshold)) %>%
    arrange(predicted) %>%
    mutate(y=cumsum(response) / row_number()) %>%
    mutate(x=row_number() / n())
  folds <- unique(bttp$fold) 
  true <- filter(bttp, method == 'true')
  true <- true[rep(1:nrow(true), times=length(folds)), ]
  true$fold <- folds
  bttp <- filter(bttp, method != 'true')
  bttp <- rbind(bttp , true)
  bttp_df <- bttp %>%
    filter(x < target_threshold) %>%
    arrange(desc(x))
  if (!is.null(base)) {
    base_bttp <- bttp_df %>%
      filter(method == base) %>%
      group_by(fold) %>%
      summarise(y=first(y))
    bttp_df <- merge(bttp_df, base_bttp, by='fold') %>%
      mutate(y=(y.x - y.y)/y.y) %>%
      filter(method != base) %>%
      group_by(method, fold)
  }
  bttp_df <- bttp_df %>%
    arrange(desc(x)) %>%
    summarise(y=first(y))
  select(bttp_df, -one_of('threshold'))
}

plot_reach_vs_waste_ <- function(joined, THRESHOLD=DEFAULT_THRESHOLDS, SHOW_CUTOFFS = FALSE, SHOW_FOLDS=FALSE, POINT_COUNT=200) {
  joined <- joined[rep(seq_len(nrow(joined)), each=length(THRESHOLD)), ]
  joined$threshold <- THRESHOLD
  joined <- dplyr::filter(joined, method!="true")
  
  df <- calculate_reach_vs_waste_(joined, FALSE)
  folded_df <- calculate_reach_vs_waste_(joined, TRUE)
  plot_cumulative(df=df,
                  base=NULL,
                  x_label="number of rich targeted / N",
                  y_label="number of poor targeted / N",
                  show_cutoffs=SHOW_CUTOFFS,
                  show_folds=SHOW_FOLDS,
                  folded=folded_df,
                  point_count=POINT_COUNT)
}

plot_reach_vs_waste <- function(..., THRESHOLD=DEFAULT_THRESHOLDS, SHOW_CUTOFFS = FALSE, SHOW_FOLDS=FALSE, POINT_COUNT=200) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  plot_reach_vs_waste_(joined, THRESHOLD, SHOW_CUTOFFS, SHOW_FOLDS, POINT_COUNT)
}


