#' @export

kfold_split <- function(k, y, x, id=NULL, weight=NULL, seed=NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (is.null(weight)) {
    weight <- rep(1,length(y))
  }
  if (is.null(id)) {
    id <- 1:length(y)
  }
  
  #Generating a sorted id variable to add back at the end of prediction
  assignments <- sample(rep(1:k, length.out=nrow(x)))
  id_sorted <- data.frame(id)
  id_sorted <- id_sorted[order(assignments),]
  
  splits <- lapply(1:k, function (k) { 
     list(
       x_train=x[assignments != k, ],
       y_train=y[assignments != k],
       w_train=weight[assignments != k],
       x_test=x[assignments == k, ],
       y_test=y[assignments == k],
       w_test=weight[assignments == k])})
  list(splits=splits, assignments=assignments, id_sorted=id_sorted)
}

kfold_fit <- function(kfold_splits, model_class) {
  splits <- kfold_splits$splits
  folds <- lapply(splits, function(s) do.call(model_class, s))
  folds <- lapply(folds, transform_ys)
  fits <- lapply(folds, fit)
  list(folds=folds, fits=fits, assignments=kfold_splits$assignments)
}

kfold_predict <- function(kfold_fits) {
  folds <- kfold_fits$folds
  fits <- kfold_fits$fits
  assignments <- kfold_fits$assignments
  #Order in df will be ascending in fold number, hence
  assignments <- sort(assignments)
  preds <- unlist(mapply(predict, folds, fits, SIMPLIFY=FALSE))
  trues <- unlist(lapply(folds, function(f) f$y_test))
  raws <- unlist(lapply(folds, function(f) f$y_test_raw))
  weight <- unlist(lapply(folds, function(f) f$w_test))
  df <- data.frame(predicted=preds, true=trues, raw=raws, weight=weight, fold=assignments)
  df
}

kfold_ <- function(model_class, kfold_splits) {
  kfold_fits <- kfold_fit(kfold_splits, model_class)
  kfold_predict(kfold_fits)
}

kfold <- function(k, model_class, y, x, id=NULL, weight=NULL, seed=0) {
  kfold_splits <- kfold_split(k, y, x, id, weight, seed)
  kfold_fits <- kfold_fit(kfold_splits, model_class)
  data.frame(kfold_predict(kfold_fits), kfold_splits$id_sorted)
}
