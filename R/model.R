#' #############
#' ### This file contains ML prediction functions for different algorithms,
#' ### designed to be harmonized
#' #############
#' @export

fold <- function(x_train, y_train, w_train, x_test, y_test, w_test) {
  list(x_train=x_train, y_train=y_train, w_train=w_train, x_test=x_test, y_test=y_test, w_test=w_test)
}


fit <- function(f) UseMethod("fit")


transform_ys <- function(f) UseMethod("transform_ys")
transform_ys.default <- function(f) {
  f$y_test_raw <- f$y_test
  f
}

transform_ys.classification <- function(f) {
  threshold <- f$threshold
  f$y_train <- factor(f$y_train < threshold, levels=c(TRUE, FALSE))
  f$y_test_raw <- f$y_test
  f$y_test <- factor(f$y_test < threshold, levels=c(TRUE, FALSE))
  f
}

# Regularized linear models ---------------------------

Ridge <- function() {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="ridge")
  }
}


fit.ridge <- function(f) {
  ridge_model <- glmnet::glmnet(f$x_train, f$y_train, weights=f$w_train, standardize=FALSE, alpha=0)
  cv_ridge <- glmnet::cv.glmnet(f$x_train, f$y_train, weights=f$w_train, standardize=FALSE, alpha=0, parallel=TRUE)
  ridge_model$best_lambda <- cv_ridge$lambda.min
  ridge_model
}


predict.ridge <- function(f, model) {
  predict(model, s=model$best_lambda, newx=f$x_test)
}


GroupedRidge <- function(grouping_variable, include_full=TRUE) {
  function(x_train, y_train, x_test, y_test) {
    f <- structure(fold(x_train, y_train, x_test, y_test), class="grouped_ridge")
    f$grouping_variable <- grouping_variable
    f$include_full <- include_full
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
  models <- list()
  best_lambdas <- list()

  if (f$include_full) {
    big_ridge <- glmnet::glmnet(x_mat_train, y_train, standardize=TRUE, alpha=0)
    big_best_lambda <- glmnet::cv.glmnet(x_mat_train, y_train, standardize=TRUE, alpha=0, parallel=TRUE)$lambda.min
    predictions <- predict(big_ridge, x_mat_train, s=big_best_lambda)
    x_mat_train <- cbind(x_mat_train, matrix(predictions, ncol=1, dimnames=list(NULL, "full_model_pred")))
    models$ungrouped_ <- big_ridge
    best_lambdas$ungrouped_ <- big_best_lambda
  }
  
  df <- data.frame(x_mat_train)
  df$Y <- f$y_train
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
  grouped_models <- as.list(m$ridge)
  names(grouped_models) <- group_names
  models <- append(models, grouped_models)
  
  group_lambdas <- as.list(m$best_lambda)
  names(group_lambdas) <- group_names
  best_lambdas <- append(best_lambdas, group_lambdas)
  
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
  if (f$include_full) {
    predictions <- predict(models$ungrouped_, x_mat_test, s=best_lambdas$ungrouped_)
    x_mat_test <- cbind(x_mat_test, matrix(predictions, ncol=1, dimnames=list(NULL, "full_model_pred")))
  }
  
  df <- data.frame(x_mat_test)
  df[, grouping_variable] <- f$x_test[, grouping_variable]
  df$id <- 1:nrow(df)
  grouped <- dplyr::group_by_(df, grouping_variable)
  to_matrix <- function(df) {
    df <- dplyr::select(df, -one_of(grouping_variable, "id"))
    as.matrix(df)
  }
  preds <- do(grouped, id=.$id, pred=predict(
    models[[ .[[grouping_variable]][[1]] ]],
    to_matrix(.),
    s=best_lambdas[[ .[[grouping_variable]][[1]] ]]))
  (data.frame(id=unlist(preds$id), pred=unlist(preds$pred)) %>% arrange(id))$pred
}


Lasso <- function(max_covariates=NULL) {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    f <- structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="lasso")
    f$max_covariates <- max_covariates
    f
  }
}


fit.lasso <- function(f) {
  lasso_model <- glmnet::glmnet(f$x_train, f$y_train, weights=f$w_train, alpha=1, standardize=TRUE)
  cv_lasso <- glmnet::cv.glmnet(f$x_train, f$y_train, weights=f$w_train, alpha=1, standardize=TRUE, parallel=TRUE)
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
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="least_squares")
  }
}


fit.least_squares <- function(f) {
  #glmnet::glmnet(f$x_train, f$y_train, standardize=FALSE, lambda=0)
  yx_train <- data.frame(Y=f$y_train, f$x_train)
  lm(Y ~ ., data=yx_train, weights=f$w_train)
}


predict.least_squares <- function(f, model) {
  predict(model, data.frame(f$x_test))
}

# Quantile regression

QuantileRegression <- function(tau=0.5) {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    f <- structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="quantile_regression")
    f$tau <- tau
    f
  }
}

fit.quantile_regression <- function(f) {
  rowc <- nrow(f$x_train)
  colc <- ncol(f$x_train)
  f$x_train <- f$x_train + matrix(rnorm(rowc * colc, mean=0, sd=.05), nrow=rowc, ncol=colc)
  yx_train <- data.frame(Y=f$y_train, f$x_train)
  quantreg::rq(Y ~ ., yx_train, tau=f$tau,weights=f$w_train)
}

predict.quantile_regression <- function(f, model) {
  predict(model, newdata=data.frame(f$x_test))
}

# Subset selection linear models ---------------------------


Stepwise <- function(max_covariates=100) {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    f <- structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="stepwise")
    f$max_covariates <- max_covariates
    f
  }
}

fit.stepwise <- function(f) {
  leaps::regsubsets(f$x_train, f$y_train, weights=f$w_train, method="forward", nvmax=f$max_covariates)
}

predict.stepwise <- function(f, model) {
  predict(model, f$x_test)
}

# Tree based models --------------------------------------

rTree <- function() {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="rTree")
  }
}

fit.rTree <- function(f) {
  yx_train <- data.frame(Y=f$y_train, f$x_train)
  tree.first <- tree::tree(Y~.,yx_train, weights=f$w_train)
  cv.trees <- tree::cv.tree(tree.first)
  #Chooses tree size with minimal deviance
  bestsize <- cv.trees$size[which.min(cv.trees$dev)]
  tree.final <- tree::prune.tree(tree.first, best = bestsize)  
}

predict.rTree <- function(f, model) {
  predict(model, f$x_test)
}

rTree2 <- function() {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="rTree2")
  }
}

fit.rTree2 <- function(f) {
  yx_train <- data.frame(Y=f$y_train,f$x_train)
  #Setting cp low to ensure trees sufficiently complex
  tree.first <- rpart::rpart(Y~., weights=f$w_train , method="anova", data=yx_train, cp=0.001)
  #Chooses tree size with minimal xerror
  bestsize <- tree.first$cptable[which.min(tree.first$cptable[,"xerror"]),"CP"]
  tree.final <- rpart::prune(tree.first, cp = bestsize)  
}

predict.rTree2 <- function(f, model) {
  predict(model, data.frame(f$x_test))
}

#No simple way to add weights to randomforest
Forest <- function() {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="forest")
  }
}

fit.forest <- function(f) {
#Supposedly this doesn't need CV  
  randomForest::randomForest(f$x_train, f$y_train, ntree=500)
}

predict.forest <- function(f, model) {
  predict(model, f$x_test)
}

BoostedTrees <- function(n.trees=500, interaction.depth=4, shrinkage=.001, distribution="gaussian") {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    f <- structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="btrees")
    f$n.trees <- n.trees
    f$interaction.depth <- interaction.depth
    f$shrinkage <- shrinkage
    f$distribution <- distribution
    f
  }
}

fit.btrees <- function(f) {
  yx_train <- data.frame(Y=f$y_train, f$x_train)
  gbm::gbm(Y ~ .,
           data=yx_train,
           interaction.depth=f$interaction.depth,
           n.trees=f$n.trees,
           shrinkage=f$shrinkage,
           distribution=f$distribution)
}

predict.btrees <- function(f, model) {
  predict(model, newdata=data.frame(f$x_test), n.trees=f$n.trees)
}

# Classification -----------------------------------------

LogisticLasso <- function(threshold) {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    f <- fold(x_train, y_train, w_train, x_test, y_test, w_test)
    f$threshold <- threshold
    structure(f, class=c("logistic_lasso", "logistic"))
  }
}


fit.logistic_lasso <- function(f) {
  model <- glmnet::glmnet(f$x_train, f$y_train, weights=f$w_train, family="binomial", standardize=FALSE, alpha=1)
  cv_out <- glmnet::cv.glmnet(f$x_train, f$y_train, weights=f$w_train, family="binomial", standardize=FALSE, alpha=1)
  model$best_lambda <- cv_out$lambda.min
  model
}

predict.logistic_lasso <- function(f, model) {
  predict(model, f$x_test, type="response", s=model$best_lambda)
}


Logistic <- function(threshold) {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    f <- fold(x_train, y_train, w_train, x_test, y_test, w_test)
    f$threshold <- threshold
    structure(f, class=c("logistic", "classification"))
  }
}


fit.logistic <- function(f) {
  glmnet::glmnet(f$x_train, f$y_train, weights=f$w_train, family="binomial", standardize=FALSE)
}

predict.logistic <- function(f, model) {
  predict(model, f$x_test, type="response", s=0)
}

# KNN Methods -------------------------------------------

MCA_KNN <- function(ndim=5, k=1, threshold=NULL) {
  function(x_train, y_train, x_test, y_test) {
    factors <- sapply(x_train, is.factor)
    if (!all(factors)) {
      print(paste("Warning:", sum(!factors), "non-factor variables found, only factors will be used."))
    }
    f <- fold(x_train, y_train, x_test, y_test)
    f$k <- k
    f$ndim <- ndim
    structure(f, class=c("mca", "knn"))
  }
}

#Haven't yes incorporated weights
PCA_KNN <- function(ndim=5, k=1, threshold=NULL) {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    numerics <- sapply(x_train, is.numeric)
    if (!all(numerics)) {
      print(paste("Warning:", sum(!numerics), "non-numeric variables found, only numerics will be used."))
    }
    f <- fold(x_train, y_train, w_train, x_test, y_test, w_test)
    f$k <- k
    f$ndim <- ndim
    structure(f, class=c("pca", "knn"))
  }
}

fit.mca <- function(f) {
  categorical <- f$x_train[, sapply(f$x_train, is.factor)]
  res.mca <- FactoMineR::MCA(categorical, ncp=f$ndim, graph=FALSE)
  res.mca
}

fit.pca <- function(f) {
  if (class(f$x_train) == "data.frame") {
    numerics <- f$x_train[, sapply(f$x_train, is.numeric)]
  }
  else {numerics <- f$x_train}
  res.pca <- FactoMineR::PCA(numerics, ncp=f$ndim, graph=FALSE)
  res.pca
}

predict.mca <- function(f, model) {
  x_test <- f$x_test[, sapply(f$x_test, is.factor)]
  var_coords <- t(data.frame(model$var$coord))
  ndim <- f$ndim
  to_model_coords <- function(obs) {
    coord <- rep(0, ndim)
    for (key in names(obs)) {
      val <- obs[key]
      if (! paste(key, val, sep="_") %in% colnames(var_coords)) {
        print(paste(key, val))
      }
      weights <- var_coords[, paste(key, val, sep="_")]
      coord <- coord + weights
    }
    coord
  }
  test_coords <- t(apply(x_test, 1, to_model_coords))
  predict_knn(f, model, test_coords)
}

predict.pca <- function(f, model) {
  if (class(f$x_test) == "data.frame") {
    x_test <- f$x_test[, sapply(f$x_test, is.numeric)]
  }
  else { x_test <- f$x_test}
  var_coords <- t(data.frame(model$var$coord))
  ndim <- nrow(var_coords)
  to_model_coords <- function(obs) {
    coord <- rep(0, ndim)
    for (key in names(obs)) {
      val <- obs[[key]]
      weights <- val * var_coords[, key]
      coord <- coord + weights
    }
    coord
  }
  test_coords <- t(apply(x_test, 1, to_model_coords))
  predict_knn(f, model, test_coords)
}


predict_knn <- function(f, model, test_coords) {
  threshold <- f$threshold
  k <- f$k
  obs_coords <- model$ind$coord
  if (is.null(threshold)) {
    winners <- class::knn(obs_coords, test_coords, 1:nrow(obs_coords))
    winners <- as.numeric(levels(winners))[as.integer(winners)]
    f$y_train[winners]
  }
  else {
    class::knn(obs_coords, test_coords, f$y_train < threshold, k=k)
  }
}

# Classification trees ----------------------------------

cTree2 <- function(threshold) {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    f <- fold(x_train, y_train, w_train, x_test, y_test, w_test)
    f$threshold <- threshold
    structure(f, class=c("cTree2", "classification"))
  }
}

fit.cTree2 <- function(f) {
  yx_train <- data.frame(Y=f$y_train,f$x_train)
  #Setting cp low to ensure trees sufficiently complex
  tree.first <- rpart::rpart(Y~., weights=f$w_train, method="class", data=yx_train, cp=0.001)
  #Chooses tree size with minimal xerror
  bestsize <- tree.first$cptable[which.min(tree.first$cptable[,"xerror"]),"CP"]
  tree.final <- rpart::prune(tree.first, cp = bestsize)  
}

predict.cTree2 <- function(f, model) {
  temp<-predict(model, data.frame(f$x_test), type = "prob")
  prob_non_poor <- temp[,2]
}


cForest <- function(threshold) {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    f <- fold(x_train, y_train, w_train, x_test, y_test, w_test)
    f$threshold <- threshold
    structure(f, class=c("cforest", "classification"))
  }
}

fit.cforest <- fit.forest

predict.cforest <- function(f, model) {
  temp<-predict(model, f$x_test, type = "prob")
  prob_non_poor <- temp[,2]
}

cBoostedTrees <- function(threshold, n.trees=500, interaction.depth=4, shrinkage=.001, distribution="bernoulli") {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    f <- structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class=c("cbtrees", "classification"))
    f$threshold <- threshold
    f$n.trees <- n.trees
    f$interaction.depth <- interaction.depth
    f$shrinkage <- shrinkage
    f$distribution <- distribution
    f
  }
}

fit.cbtrees <- function(f) {
  yx_train <- data.frame(Y=f$y_train, f$x_train)
  gbm::gbm(Y ~ .,
           data=yx_train,
           interaction.depth=f$interaction.depth,
           n.trees=f$n.trees,
           shrinkage=f$shrinkage,
           distribution=f$distribution)
}

predict.cbtrees <- function(f, model) {
  predict(model, newdata=data.frame(f$x_test), n.trees=f$n.trees, type="response")
}
