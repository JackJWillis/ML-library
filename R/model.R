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
  ridge_model <- glmnet::glmnet(f$x_train, f$y_train, standardize=FALSE, alpha=0)
  cv_ridge <- glmnet::cv.glmnet(f$x_train, f$y_train, standardize=FALSE, alpha=0, parallel=TRUE)
  ridge_model$best_lambda <- cv_ridge$lambda.min
  ridge_model
}


predict.ridge <- function(f, model) {
  predict(model, s=model$best_lambda, newx=f$x_test)
}


GroupedRidge <- function(grouping_variable) {
  function(x_train, y_train, x_test, y_test) {
    f <- structure(fold(x_train, y_train, x_test, y_test), class="grouped_ridge")
    f$grouping_variable <- grouping_variable
    f
  }
}

fit.grouped_ridge <- function(f) {
  stopifnot(!("Y" %in% colnames(f$x_train)))
  grouping_variable <- f$grouping_variable
  all_x <- rbind(f$x_train, f$x_test)
  all_y <- c(f$y_train, f$y_test)
  all_df <- data.frame(Y=all_y, all_x)
  all_df <- dplyr::select(all_df, -one_of(grouping_variable))
  x_mat <- model.matrix(Y ~ ., all_df)
  
  x_mat_train <- x_mat[1:nrow(f$x_train), ] 
  y_train <- f$y_train
  big_ridge <- glmnet::glmnet(x_mat_train, y_train, standardize=TRUE, alpha=0)
  big_best_lambda <- glmnet::cv.glmnet(x_mat_train, y_train, standardize=TRUE, alpha=0, parallel=TRUE)$lambda.min
  residuals <- predict(big_ridge, x_mat_train, s=big_best_lambda) - y_train
  
  df <- data.frame(x_mat_train)
  df$Y <- residuals
  df[, grouping_variable] <- f$x_train[, grouping_variable]
  grouped <- dplyr::group_by_(df, grouping_variable)
  to_matrix <- function(df) {
    df <- dplyr::select(df, -one_of("Y", grouping_variable))
    as.matrix(df)
  }
  m <- do(grouped,
          ridge=glmnet::glmnet(to_matrix(.), .$Y, standardize=TRUE, alpha=0),
          best_lambda=glmnet::cv.glmnet(to_matrix(.), .$Y, standardize=TRUE, alpha=0, parallel=TRUE)$lambda.min)
  
  group_names <- m[[grouping_variable]]
  models <- as.list(m$ridge)
  names(models) <- group_names
  models$ungrouped_ <- big_ridge
  
  best_lambdas <- as.list(m$best_lambda)
  names(best_lambdas) <- group_names
  best_lambdas$ungrouped_ <- big_best_lambda
  
  list(ridge=models, best_lambdas=best_lambdas)
}

predict.grouped_ridge <- function(f, model) {
  models <- model$ridge
  best_lambdas <- model$best_lambdas
  grouping_variable <- f$grouping_variable
  
  all_x <- rbind(f$x_train, f$x_test)
  all_y <- c(f$y_train, f$y_test)
  all_df <- data.frame(Y=all_y, all_x)
  all_df <- dplyr::select(all_df, -one_of(f$grouping_variable))
  x_mat <- model.matrix(Y ~ ., all_df)
  
  x_mat_test <- x_mat[(nrow(f$x_train)+1):nrow(x_mat), ] 
  pred <- predict(models$ungrouped_, x_mat_test, s=best_lambdas$ungrouped_)
  
  df <- data.frame(x_mat_test)
  df[, grouping_variable] <- f$x_test[, grouping_variable]
  df$id <- 1:nrow(df)
  grouped <- dplyr::group_by_(df, grouping_variable)
  to_matrix <- function(df) {
    df <- dplyr::select(df, -one_of(grouping_variable, "id"))
    as.matrix(df)
  }
  residuals <- do(grouped, id=.$id, res=predict(
    models[[ .[[grouping_variable]][[1]] ]],
    to_matrix(.),
    s=best_lambdas[[ .[[grouping_variable]][[1]] ]]))
  residuals <- (data.frame(id=unlist(residuals$id), res=unlist(residuals$res)) %>% arrange(id))$res
  pred + residuals
}


Lasso <- function(max_covariates=NULL) {
  function(x_train, y_train, x_test, y_test) {
    f <- structure(fold(x_train, y_train, x_test, y_test), class="lasso")
    f$max_covariates <- max_covariates
    f
  }
}


fit.lasso <- function(f) {
  lasso_model <- glmnet::glmnet(f$x_train, f$y_train, alpha=1, standardize=TRUE)
  cv_lasso <- glmnet::cv.glmnet(f$x_train, f$y_train, alpha=1, standardize=TRUE, parallel=TRUE)
  max_covariates <- f$max_covariates
  if (is.null(max_covariates)) {
    s <- cv_lasso$lambda.min
  }
  else {
    s <- cv_lasso$lambda[which.min(cv_lasso$cvm[cv_lasso$nzero < max_covariates])]
  }
  lasso_model$best_lambda <- s 
  lasso_model
}


predict.lasso <- function(f, model) {
  predict(model, newx=f$x_test, s=model$best_lambda)
}


LeastSquares <- function() {
  function(x_train, y_train, x_test, y_test) {
    structure(fold(x_train, y_train, x_test, y_test), class="least_squares")
  }
}


fit.least_squares <- function(f) {
  #glmnet::glmnet(f$x_train, f$y_train, standardize=FALSE, lambda=0)
  yx_train_global <- data.frame(Y=f$y_train,f$x_train)
  names(yx_train_global)[1] <-"Y"
  lm(Y ~ ., data=yx_train_global)
}


predict.least_squares <- function(f, model) {
  predict(model, data.frame(f$x_test))
}


# Subset selection linear models ---------------------------

predict.regsubsets=function(object, newdata, ...){
  id <- which.min(summary(object)$rss)
  coefficients <- coef(object, id=id)
  xvars <- names(coefficients)
  newdata[, xvars] %*% coefficients
}

Stepwise <- function(max_covariates=100) {
  function(x_train, y_train, x_test, y_test) {
    f <- structure(fold(x_train, y_train, x_test, y_test), class="stepwise")
    f$max_covariates <- max_covariates
    f
  }
}

fit.stepwise <- function(f) {
  leaps::regsubsets(f$x_train, f$y_train, method="forward", nvmax=f$max_covariates)
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

rTree2 <- function() {
  function(x_train, y_train, x_test, y_test) {
    structure(fold(x_train, y_train, x_test, y_test), class="rTree2")
  }
}

fit.rTree2 <- function(f) {
  yx_train_global <<- data.frame(Y=f$y_train,f$x_train)
  names(yx_train_global)[1]<<-"Y"
  #Setting cp low to ensure trees sufficiently complex
  tree.first <- rpart::rpart(Y~., method="anova", data=yx_train_global, cp=0.001)
  #Chooses tree size with minimal xerror
  bestsize <- tree.first$cptable[which.min(tree.first$cptable[,"xerror"]),"CP"]
  tree.final <- rpart::prune(tree.first, cp = bestsize)  
}

predict.rTree2 <- function(f, model) {
  predict(model, data.frame(f$x_test))
}

Forest <- function() {
  function(x_train, y_train, x_test, y_test) {
    structure(fold(x_train, y_train, x_test, y_test), class="forest")
  }
}

fit.forest <- function(f) {
#Supposedly this doesn't need CV  
  randomForest::randomForest(f$x_train, f$y_train, ntree=500)
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

kfold_split <- function(k, y, x, seed=NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  assignments <- sample(rep(1:k, length.out=nrow(x)))
  splits <- lapply(1:k, function (k) { 
     list(
       x_train=x[assignments != k, ],
       y_train=y[assignments != k],
       x_test=x[assignments == k, ],
       y_test=y[assignments == k])})
  list(splits=splits, assignments=assignments)
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
  df <- data.frame(predicted=preds, true=trues, raw=raws, fold=assignments)
  df
}

kfold <- function(k, model_class, y, x, seed=0) {
  kfold_splits <- kfold_split(k, y, x, seed)
  kfold_fits <- kfold_fit(kfold_splits, model_class)
  kfold_predict(kfold_fits)
}


