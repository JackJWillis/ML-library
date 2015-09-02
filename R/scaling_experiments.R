
get_test_train_assignments <- function(count, holdout_fraction) {
  as.logical(rbinom(count, 1, 1 - holdout_fraction))
}

kfold_format <- function(splits, assignments, ids) {
  list(splits=splits, assignments=assignments, id_sorted=ids)
}

scale_n <- function(y, x, holdout_fraction=.2, holdout_assignments=NULL, steps=10, model_class=LeastSquares()) {
  if (is.null(holdout_assignments)) {
    holdout_assignments <- get_test_train_assignments(length(y), holdout_fraction)
  }
  y_holdout <- y[!holdout_assignments]
  x_holdout <- x[!holdout_assignments, ]
  y_train <- y[holdout_assignments]
  x_train <- x[holdout_assignments, ]
  
  chunk_assignments <- sort(seq_along(y_train) %% steps)
  train_order <- sample(seq_along(y_train))
  
  
  test_train_splits <- lapply(1:steps, function(i) {
    in_batch <- chunk_assignments < i
    batch_size <- sum(in_batch)
    list(
      y_train=y_train[train_order][in_batch],
      x_train=x_train[train_order, ][in_batch, ],
      w_train=rep(1, batch_size),
      y_test=y_holdout,
      x_test=x_holdout,
      w_test=rep(1, length(y_holdout)))
  })
  
  ids <- rep(seq_along(y_holdout), times=steps)
  assignments <- sort(rep(1:steps, times=length(y_holdout)))
  test_train_splits <- list(splits=test_train_splits, assignments=assignments, id_sorted=ids)
  df <- kfold_(model_class, test_train_splits)$pred
  df$method <- 'n'
  df
}


scale_k <- function(y, x, holdout_fraction=0.2, holdout_assignments=NULL, steps=NULL, method='stepwise') {
  if (is.null(holdout_assignments)) {
    holdout_assignments <- get_test_train_assignments(length(y), holdout_fraction)
  }
  y_holdout <- y[!holdout_assignments]
  x_holdout <- x[!holdout_assignments, ]
  y_train <- y[holdout_assignments]
  x_train <- x[holdout_assignments, ]
  if (is.null(steps)) {
    steps <- ncol(x_train)
  }
  
  yx_train <- data.frame(Y=y_train, x_train)
  if (method == 'stepwise') {
    model <- leaps::regsubsets(Y ~ ., data=yx_train, method="forward", nvmax=ncol(x_train))
    captured <- nrow(summary(model)$which)
    fits <- lapply(round(seq(1, captured, length=steps)), function(i) {
      features <- summary(model)$which[i, ]
      features <- features[-1]
      lm(Y ~ ., data=yx_train[, c(TRUE, features)])
    })
    preds <- unlist(lapply(fits, function(f) predict(f, data.frame(x_holdout))))
    assignments <- sort(rep(1:steps, times=length(y_holdout)))
  }
  if (method == 'lasso') {
    model <- glmnet::glmnet(x_train, y_train, alpha=1)
    nzero <- colSums(abs(model$beta) > 0)
    positions <- match(unique(nzero), nzero)
    assignments <- sort(rep(nzero[positions], times=length(y_holdout)))
    preds <- unlist(lapply(model$lambda[positions], function(s) predict(model, x_holdout, s=s)))
    steps <- length(positions)
  }
  ids <- rep(seq_along(y_holdout), times=steps)
  trues<- rep(y_holdout, times=steps)
  df <- data.frame(predicted=preds, true=trues, raw=trues, weight=1, fold=assignments, id=ids)
  df$method <- 'k'
  df
}


DATASETS <- c('tanzania', 'ghana_pe', 'niger_pastoral', 'niger_agricultural')
plot_all <- function(datasets=DATASETS, steps=20) {
  datas <- lapply(datasets, function(ds_name) {
    data <- load_dataset(ds_name)
    data <- select(data, -one_of('X'))
    if (grepl('niger', ds_name)) {
      x <- model.matrix(y_real ~ ., data)
      y <- data[rownames(x), 'y_real']
    } else if (grepl('ghana', ds_name)) {
      x <- model.matrix(lnwelfare ~ ., data)
      y <- data[rownames(x), 'lnwelfare']
    } else {
      x <- model.matrix(lconsPC ~ ., data)
      y <- data[rownames(x), 'lconsPC']
    }
    list(x=x, y=y)
  })
  names(datas) <- datasets
  ensembles <- lapply(DATASETS, function(ds_name) {
    joined <- load_models(ds_name)
    e <- filter(joined, method == 'ensemble_all')
    calculate_reach_(e)$reach})
  ensembles <- data.frame(ensemble=datasets, y=unlist(ensembles))
  k_results <- lapply(datasets, function(ds_name) {
    print(ds_name)
    d <- datas[[ds_name]]
    x <- d$x
    y <- d$y
    df <- calculate_reach_(scale_k(y, x), fold=TRUE)
    df$dataset <- ds_name
    df
  })
  k_results <- do.call('rbind', k_results)
  column_counts <- group_by(k_results, dataset) %>% summarize(column_count=max(fold))
  ensembles <- merge(ensembles, column_counts, by.x='ensemble', by.y='dataset')
  k_plot <- ggplot(k_results, aes(x=fold, y=reach, color=dataset)) +
    geom_line() +
    xlab('column count') +
    ylab('reach') + 
    geom_point(data=ensembles, mapping=aes(y=y, color=ensemble, x=column_count))
  
  lasso_k_results <- lapply(datasets, function(ds_name) {
    print(ds_name)
    d <- datas[[ds_name]]
    x <- d$x
    y <- d$y
    df <- calculate_reach_(scale_k(y, x, method='lasso'), fold=TRUE)
    df$dataset <- ds_name
    df
  })
  lasso_k_results <- do.call('rbind', lasso_k_results)
  lasso_k_plot <- ggplot(lasso_k_results, aes(x=fold, y=reach, color=dataset)) +
    geom_line() +
    xlab('column count') +
    ylab('reach') +
    geom_point(data=ensembles, mapping=aes(y=y, color=ensemble, x=column_count))
  
  n_results <- lapply(datasets, function(ds_name) {
    print(ds_name)
    d <- datas[[ds_name]]
    x <- d$x
    y <- d$y
    df <- calculate_reach_(scale_n(y, x, steps=steps), fold=TRUE)
    df$dataset <- ds_name
    df$x <- (df$fold / max(df$fold)) * nrow(x) * 0.8
    df
  })
  n_results <- do.call('rbind', n_results)
  n_plot <- ggplot(n_results, aes(x=x, y=reach, color=dataset)) +
    geom_line() +
    xlab('rows') +
    ylab('reach') +
    scale_x_log10()
#     geom_point(data=ensembles, mapping=aes(y=y, color=ensemble, x=1))
  
  ggsave('scale_n.pdf', n_plot)
  gsave('scale_k.pdf', k_plot)
  ggsave('scale_k_lasso.pdf', lasso_k_plot)
}

MODELS <- list(ols=LeastSquares(), rtree=rTree(), lasso=Lasso(), forest=Forest())
plot_models<- function(ds_name, models=MODELS, steps=20) {
  data <- load_dataset(ds_name)
  data <- select(data, -one_of('X'))
  if (grepl('niger', ds_name)) {
    x <- model.matrix(y_real ~ ., data)
    x_nmm <- select(data, -one_of('y_real'))
    y <- data[rownames(x), 'y_real']
  } else if (grepl('ghana', ds_name)) {
    x <- model.matrix(lnwelfare ~ ., data)
    x_nmm <- select(data, -one_of('lnwelfare'))
    y <- data[rownames(x), 'lnwelfare']
  } else {
    x <- model.matrix(lconsPC ~ ., data)
    x_nmm <- select(data, -one_of('lconsPC'))
    y <- data[rownames(x), 'lconsPC']
}
  n_results <- lapply(names(models), function(m_name) {
    print(m_name)
    model <- models[[m_name]]
    if (m_name == 'rtree' | m_name == 'forest') {
      preds <- scale_n(y, x_nmm, model_class=model, steps=steps)
    }
    else {
      preds <- scale_n(y, x, model_class=model, steps=steps)
    }
    df <- calculate_reach_(preds, fold=TRUE)
    df$method <- m_name
    df$x <- df$fold / max(df$fold)
    df
  })
  n_results <- do.call('rbind', n_results)
  n_plot <- ggplot(n_results, aes(x=x, y=reach, color=method)) +
    geom_line() +
    xlab('fraction of total rows') +
    ylab('reach')
  ggsave(paste(ds_name, '.pdf', sep=''), n_plot)
}