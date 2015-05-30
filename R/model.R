#' #############
#' ### This file contains ML prediction functions for different algorithms,
#' ### designed to be harmonized
#' #############
#' @export

# Data processing ---------------------------

standardize_predictors  <-  function(df, target) {
  constant_or_empty <- function(values) {
    all(is.na(values)) || all(values[1] == values)
  }
  standard <- dplyr::select(df, -matches(target))

  numeric_features <- sapply(standard, is.numeric)
  standard[, numeric_features] <- scale(standard[, numeric_features], center=TRUE, scale=TRUE)

  degenerate <- sapply(standard, constant_or_empty)
  standard <- standard[, !degenerate]

  standard[, target] <- df[, target]
  standard
}

MISSINGNESS_INDICATOR <- "missing_missing_missing"
na_indicator <- function(df) {
  for (name in names(df)) {
    if (any(is.na(df[[name]]))) {
      if (is.factor(df[[name]])) {
        levels(df[[name]]) <- c(levels(df[[name]]), MISSINGNESS_INDICATOR)
        df[is.na(df[[name]]), name] <- MISSINGNESS_INDICATOR
      }
      if (is.numeric(df[[name]])) {
        df[[paste(name, "NA", sep=".")]] <- is.na(df[[name]])
        df[is.na(df[[name]]), name] <- 0
      }
    }
  }
  df
}


fold <- function(x_train, y_train, x_test, y_test) {
  list(x_train=x_train, y_train=y_train, x_test=x_test, y_test=y_test)
}


fit <- function(f) UseMethod("fit")

transform_ys <- function(f) UseMethod("transform_ys")
transform_ys.default <- function(f) {
  f$y_test_raw <- f$y_test
  f
}

# Regularized linear models ---------------------------

Ridge <- function() {
  function(x_train, y_train, x_test, y_test) {
    structure(fold(x_train, y_train, x_test, y_test), class="ridge")
  }
}


fit.ridge <- function(f) {
  glmnet::cv.glmnet(f$x_train, f$y_train, standardize=FALSE, alpha=0, parallel=TRUE)
}


predict.ridge <- function(f, model) {
  predict(model, f$x_test)
}


Lasso <- function() {
  function(x_train, y_train, x_test, y_test) {
    structure(fold(x_train, y_train, x_test, y_test), class="lasso")
  }
}


fit.lasso <- function(f) {
  glmnet::cv.glmnet(f$x_train, f$y_train, standardize=FALSE, alpha=1, parallel=TRUE)
}


predict.lasso <- function(f, model) {
  predict(model, f$x_test)
}


LeastSquares <- function() {
  function(x_train, y_train, x_test, y_test) {
    structure(fold(x_train, y_train, x_test, y_test), class="least_squares")
  }
}


fit.least_squares <- function(f) {
  glmnet::glmnet(f$x_train, f$y_train, standardize=FALSE, lambda=0)
}


predict.least_squares <- function(f, model) {
  predict(model, f$x_test)
}


# Subset selection linear models ---------------------------

predict.regsubsets=function(object, newdata, ...){
  id <- which.min(summary(object)$bic)
  coefficients <- coef(object, id=id)
  xvars <- names(coefficients)
  newdata[, xvars] %*% coefficients
}

Stepwise <- function() {
  function(x_train, y_train, x_test, y_test) {
    structure(fold(x_train, y_train, x_test, y_test), class="stepwise")
  }
}

fit.stepwise <- function(f) {
  leaps::regsubsets(f$x_train, f$y_train, method="forward", nvmax=100)
}

predict.stepwise <- function(f, model) {
  predict(model, f$x_test)
}

# Tree based models --------------------------------------

rTree <- function() {
  function(x_train, y_train, x_test, y_test) {
    structure(fold(x_train, y_train, x_test, y_test), class="rTree")
  }
}

fit.rTree <- function(f) {
  yx_train_global <<- data.frame(Y=f$y_train,f$x_train)
  names(yx_train_global)[1]<<-"Y"
  tree.first <- tree::tree(Y~.,yx_train_global)
  cv.trees <- tree::cv.tree(tree.first)
  #Chooses tree size with minimal deviance
  bestsize <- cv.trees$size[which.min(cv.trees$dev)]
  tree.final <- tree::prune.tree(tree.first, best = bestsize)  
}

predict.rTree <- function(f, model) {
  predict(model, f$x_test)
}

Forest <- function() {
  function(x_train, y_train, x_test, y_test) {
    structure(fold(x_train, y_train, x_test, y_test), class="forest")
  }
}

fit.forest <- function(f) {
#Supposedly this doesn't need CV  
  randomForest::randomForest(f$x_train, f$y_train)
}

predict.forest <- function(f, model) {
  predict(model, f$x_test)
}

# Classification -----------------------------------------

Logistic <- function(threshold) {
    function(x_train, y_train, x_test, y_test) {
      f <- fold(x_train, y_train, x_test, y_test)
      f$threshold <- threshold
      structure(f, class="logistic")
    }
}

transform_ys.logistic <- function(f) {
  threshold <- f$threshold
  f$y_train <- factor(f$y_train < threshold, levels=c(TRUE, FALSE))
  f$y_test_raw <- f$y_test
  f$y_test <- factor(f$y_test < threshold, levels=c(TRUE, FALSE))
  f
}

fit.logistic <- function(f) {
  glmnet::cv.glmnet(f$x_train, f$y_train, family="binomial")
}

predict.logistic <- function(f, model) {
  predict(model, f$x_test, type="response", lambda=lambda.min)
}


# K fold validation ---------------------------

kfold <- function(k, model_class, y, x, seed=0) {
  set.seed(seed)
  assignments <- sample(1:k, nrow(x), replace=TRUE) #TODO load balance
  folds <- lapply(1:k, function (k) { 
    model_class(x[assignments != k, ], y[assignments != k], x[assignments == k, ], y[assignments == k])})
  folds <- lapply(folds, transform_ys)
  fits <- lapply(folds, fit)
  preds <- unlist(mapply(predict, folds, fits, SIMPLIFY=FALSE))
  trues <- unlist(lapply(folds, function(f) f$y_test))
  raws <- unlist(lapply(folds, function(f) f$y_test_raw))
  df <- data.frame(predicted=preds, true=trues, raw=raws, fold=assignments)
  df
}
# ###Regression Tree
# 
# library(tree)
# 
# tree.predict = function(yx_train,yx_test){
#   #Have to generate a global version otherise cv.trees doesn't ork. See here:
#   #http://stackoverflow.com/questions/28148533/is-data-framedata-object-not-found-in-function-context
#   yx_train_global <<- yx_train
#   names(yx_train_global)[1]<<-"Y"
#   tree.first <- tree(Y~.,yx_train_global)
#   cv.trees <- cv.tree(tree.first)
#   #Chooses tree size with minimal deviance
#   bestsize <- cv.trees$size[which.min(cv.trees$dev)]
#   tree.final <- prune.tree(tree.first, best = bestsize)
#   pred=predict(tree.final,newdata=yx_test[,-1])
#   pred.tree=predict(tree.final,newdata=yx_test[,-1],type="tree")
#   MSE=mean((pred-yx_test[[1]])^2)
#   out=data.frame(y_pred=pred,y_real=yx_test[[1]])
#   tree=list(out=out,tree=pred.tree,MSE=MSE,bestsize=bestsize)
#   yx_train_global <<- NA
#   return(tree)  
# }
# 
# 
# #Function which will run cross validated output on whole data
# tree.predict.kfold = function(yx,k,s){
#   set.seed(s) 
#   folds=sample(1:k,nrow(yx),replace=TRUE)
#   tree.folds <- list()
#   for(j in 1:k){
#     tree.folds[[j]] <- tree.predict(yx[folds!=j,],yx[folds==j,])
#     names(tree.folds)[j] <- paste("fold_",j,sep="")
#   }
#   return(tree.folds)
# }
# 
# 
# ###Random forests
# 
# library(randomForest)
# 
#   # TO FILL
# 
# 
# ###Conditional inference trees
# 
# library(party)
# 
# CIT.predict = function(yx_train,yx_test){
#   names(yx_train)[1]<-"Y"
#   fit <- ctree(Y~.,data=yx_train)
#   
#   # TO FILL  
# }
# 
