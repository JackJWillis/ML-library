get_test_train_assignments <- function(count, holdout_fraction) {
  as.logical(rbinom(count, 1, 1 - holdout_fraction))
}

kfold_format <- function(splits, assignments, ids) {
  list(splits=splits, assignments=assignments, id_sorted=ids)
}

scale_n <- function(y, x, holdout_fraction=.2, holdout_assignments=NULL, steps=10) {
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
  df <- kfold_(LeastSquares(), test_train_splits)$pred
  df$method <- 'n'
  df
}

scale_k <- function(y, x, holdout_fraction=0.2, holdout_assignments=NULL, steps=20) {
  if (is.null(holdout_assignments)) {
    holdout_assignments <- get_test_train_assignments(length(y), holdout_fraction)
  }
  y_holdout <- y[!holdout_assignments]
  x_holdout <- x[!holdout_assignments, ]
  y_train <- y[holdout_assignments]
  x_train <- x[holdout_assignments, ]
  
  yx_train <- data.frame(Y=y_train, x_train)
  model <- leaps::regsubsets(Y ~ ., data=yx_train, method="forward", nvmax=ncol(x_train))
  captured <- nrow(summary(model)$which)
  fits <- lapply(round(seq(1, captured, length=steps)), function(i) {
    features <- summary(model)$which[i, ]
    features <- features[-1]
    lm(Y ~ ., data=yx_train[, c(TRUE, features)])
  })
  ids <- rep(seq_along(y_holdout), times=steps)
  assignments <- sort(rep(1:steps, times=length(y_holdout)))
  preds <- unlist(lapply(fits, function(f) predict(f, data.frame(x_holdout))))
  trues<- rep(y_holdout, times=steps)
  df <- data.frame(predicted=preds, true=trues, raw=trues, weight=1, fold=assignments, id=ids)
  df$method <- 'k'
  df
}