get_test_train_assignments <- function(count, holdout_fraction) {
  as.logical(rbinom(count, 1, 1 - holdout_fraction))
}

kfold_format <- function(splits, assignments, ids) {
  list(splits=splits, assignments=assignments, id_sorted=ids)
}

scale_n <- function(y, x, holdout_fraction=.2, steps=10) {
  assignments <- get_test_train_assignments(length(y), holdout_fraction)
  y_holdout <- y[!assignments]
  x_holdout <- x[!assignments, ]
  y_train <- y[assignments]
  x_train <- x[assignments, ]
  
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
  kfold_(LeastSquares(), test_train_splits)
}