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

to_mm <- function(f) {
  test <- data.frame(Y=f$y_test, f$x_test)
  train <- data.frame(Y=f$y_train, f$x_train)
  columns <- intersect(colnames(test), colnames(train))
  test <- test[, columns]
  train <- train[, columns]
  
  df <- rbind(test, train)
  mm <- model.matrix(Y ~ -1 + ., df)
  x_test <- mm[1:nrow(test), ]
  x_train <- mm[(nrow(test)+1):nrow(mm), ]
  list(x_test=x_test, y_test=f$y_test, x_train=x_train, y_train=f$y_train, w_test=f$w_test, w_train=f$w_train)
}

# Linear models ---------------------------

Ridge <- function() {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="ridge")
  }
}


fit.ridge <- function(f) {
  if (class(f$x_test) == "data.frame") {
    f <- to_mm(f)
  }
  ridge_model <- glmnet::glmnet(f$x_train, f$y_train, weights=f$w_train, standardize=TRUE, alpha=0)
  cv_ridge <- glmnet::cv.glmnet(f$x_train, f$y_train, weights=f$w_train, standardize=TRUE, alpha=0, parallel=TRUE)
  ridge_model$best_lambda <- cv_ridge$lambda.min
  ridge_model
}


predict.ridge <- function(f, model) {
  if (class(f$x_test) == "data.frame") {
    f <- to_mm(f)
  }
  predict(model, s=model$best_lambda, newx=f$x_test)
}


GroupedRidge <- function(grouping_variable, include_full=TRUE) {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    f <- structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="grouped_ridge")
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


PostLasso <- function(max_covariates=NULL) {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    f <- structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="post_lasso")
    f$max_covariates <- max_covariates
    f
  }
}

fit.post_lasso <- function(f) {
  addq <- function(x) paste0("`", x, "`")
  lasso_model <- fit.lasso(f)
  columns <- coef(lasso_model)[, which.max(lasso_model$lambda == lasso_model$best_lambda)]
  columns <- abs(columns) > 0
  columns <- names(columns)[columns]
  columns <- Filter(function(col) col != '(Intercept)', columns)
  glmnet::glmnet(f$x_train[, columns], f$y_train, standardize=FALSE, lambda=0)
}

predict.post_lasso <- function(f, model) {
  predict(model, newx=f$x_test[, rownames(coef(model))[-1]])
}

Lasso <- function(max_covariates=NULL) {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    f <- structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="lasso")
    f$max_covariates <- max_covariates
    f
  }
}

fit.lasso <- function(f) {
  if (class(f$x_test) == "data.frame") {
    f <- to_mm(f)
  }
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
  if (class(f$x_test) == "data.frame") {
    f <- to_mm(f)
  }
  predict(model, newx=f$x_test, s=model$best_lambda)
}


LeastSquaresPC <- function(ncomp=20) {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    x <- rbind(x_train, x_test)
    x <- prcomp(x, retx=TRUE)$x[, 1:ncomp]
    x_train <- x[1:nrow(x_train), ]
    x_test <- x[(nrow(x_train)+1):(nrow(x_train)+nrow(x_test)), ]
    structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class=c("least_squares_pc", "least_squares"))
  }
}

BinaryLeastSquares <- function(threshold) {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    f <- fold(x_train, y_train, w_train, x_test, y_test, w_test)
    f$threshold <- threshold
    structure(f, class=c("least_squares", "binary")) }
}

transform_ys.binary <- function(f) {
  threshold <- f$threshold
  f$y_train <- f$y_train > threshold
  f$y_test_raw <- f$y_test
  f$y_test <- f$y_test > threshold
  f
}




  
  

LeastSquares <- function() {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="least_squares")
  }
}


fit.least_squares <- function(f) {
  if (class(f$x_test) == "data.frame") {
    f <- to_mm(f)
  }
  #glmnet::glmnet(f$x_train, f$y_train, standardize=FALSE, lambda=0)
  yx_train <- data.frame(Y=f$y_train, f$x_train)
  lm(Y ~ ., data=yx_train, weights=f$w_train)
}


predict.least_squares <- function(f, model) {
  if (class(f$x_test) == "data.frame") {
    f <- to_mm(f)
  }
  predict(model, data.frame(f$x_test))
}

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
  quantreg::predict.rq(model, newdata=data.frame(f$x_test))
}

Stepwise <- function(max_covariates=100, original_columns=NULL) {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    f <- structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="stepwise")
    f$max_covariates <- max_covariates
    f$original_columns <- original_columns
    f
  }
}

to_original_columns <- function(mm_columns, original_columns) {
  idx <- sapply(original_columns, function(original_column) {
    any(grepl(original_column, mm_columns))
  })
  original_columns[idx]
}

fit.stepwise <- function(f) {
  if (class(f$x_test) == "data.frame") {
    mm <- to_mm(f)
  }
  yx_train <- data.frame(Y=f$y_train, mm$x_train)
  # HACK to remove linearly dependant columns
  model <- lm(Y ~ ., data=yx_train)
  yx_train2 <- yx_train[, !is.na(coef(model))]
  l <- leaps::regsubsets(Y ~ ., data=yx_train2, weights=f$w_train, method="forward", nvmax=1000)
  features <- NULL
  i <- 0
  if (!is.null(f$original_columns)) {
    original_columns <- f$original_columns
  }
  else {
    original_columns <- colnames(yx_train2)
  }
  while (length(features) < f$max_covariates) {
    i <- i + 1
    l_mm_columns <- summary(l)$which[i, ]
    mm_columns <- names(l_mm_columns)[l_mm_columns]
    features <- to_original_columns(mm_columns, original_columns)
  }
  yx_train_nmm <- data.frame(Y=f$y_train, f$x_train)
  ols <- lm(Y ~ ., data=yx_train_nmm[, c('Y', features)], weights=f$w_train)
  ols$stepwise_features <- features
  ols
}

predict.stepwise <- function(f, model) {
  predict(model, data.frame(f$x_test))
}

# Tree based models --------------------------------------

rTree <- function() {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="rTree")
  }
}

fit.rTree <- function(f) {
  yx_train <- data.frame(Y=f$y_train,f$x_train)
  #Setting cp low to ensure trees sufficiently complex
  tree.first <- rpart::rpart(Y~., weights=f$w_train , method="anova", data=yx_train, cp=0.001)
  #Chooses tree size with minimal xerror
  bestsize <- tree.first$cptable[which.min(tree.first$cptable[,"xerror"]),"CP"]
  tree.final <- rpart::prune(tree.first, cp = bestsize)  
}

predict.rTree <- function(f, model) {
  predict(model, data.frame(f$x_test))
}

TreePlusLinear <- function() {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="tree_p_linear")
  }
}

fit.tree_p_linear <- function(f) {
  yx_train <- data.frame(Y=f$y_train,f$x_train)
  columns <- paste(colnames(yx_train), collapse='+')
  fmla <- as.formula(paste('Y ~', columns, '|', columns))
  party::mob(fmla, data=yx_train, model=modeltools::linearModel)
}

predict.tree_p_linear <- function(f, model) {
  predict(model, data.frame(f$x_test))
}

LinearPlusForest <- function() {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="linear_p_forest")
  }
}

fit.linear_p_forest<- function(f) {
  if (class(f$x_test) == "data.frame") {
    mm <- to_mm(f)
  }
  yx_train <- data.frame(Y=f$y_train, mm$x_train)
  linear_part <- lm(Y ~ ., data=yx_train)
  residuals <- f$y_train - predict(linear_part, yx_train)
  nonlinear_part <- randomForest::randomForest(x=f$x_train, y=residuals, ntree=50)
  list(linear=linear_part, nonlinear=nonlinear_part)
}

predict.linear_p_forest <- function(f, model) {
  if (class(f$x_test) == "data.frame") {
    mm <- to_mm(f)
  }
  linear_part <- predict(model$linear, data.frame(mm$x_test))
  nonlinear_part <- predict(model$nonlinear, f$x_test)
  linear_part + nonlinear_part
}

#No simple way to add weights to randomforest
Forest <- function() {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="forest")
  }
}

fit.forest <- function(f) {
#   if (class(f$x_test) == "data.frame") {
#     f <- to_mm(f)
#   }
#Supposedly this doesn't need CV  
  yx_train <- data.frame(Y=f$y_train,f$x_train)
  if (nrow(f$x_train > 2000)) {
    ntree <- 50
  }
  else {
    ntree <- 200
  }
  randomForest::randomForest(x=f$x_train, y=f$y_train, ntree=ntree)
}

predict.forest <- function(f, model) {
#   if (class(f$x_test) == "data.frame") {
#     f <- to_mm(f)
#   }
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
    structure(f, class=c("logistic_lasso", "classification"))
  }
}


fit.logistic_lasso <- function(f) {
  model <- glmnet::glmnet(f$x_train, f$y_train, weights=f$w_train, family="binomial", standardize=TRUE, alpha=1)
  cv_out <- glmnet::cv.glmnet(f$x_train, f$y_train, weights=f$w_train, family="binomial", standardize=TRUE, alpha=1)
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
    structure(f, class=c("logistic", "binary"))
  }
}


fit.logistic <- function(f) {
  if (class(f$x_test) == "data.frame") {
    f <- to_mm(f)
  }
  glmnet::glmnet(f$x_train, f$y_train, weights=f$w_train, family="binomial", standardize=TRUE, lambda=0)
#   yx_train <- data.frame(Y=f$y_train, f$x_train)
#   glm(Y ~ ., data=yx_train, weights=f$w_train, family=binomial())
}

predict.logistic <- function(f, model) {
  if (class(f$x_test) == "data.frame") {
    f <- to_mm(f)
  }
  predict(model, f$x_test, type="response", s=0)
}

# KNN Methods -------------------------------------------

MCA_KNN <- function(ndim=5, k=1, threshold=NULL) {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    factors <- sapply(x_train, is.factor)
    if (!all(factors)) {
      print(paste("Warning:", sum(!factors), "non-factor variables found, only factors will be used."))
    }
    f <- fold(x_train, y_train, w_train, x_test, y_test, w_test)
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

cTree <- function(threshold) {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    f <- fold(x_train, y_train, w_train, x_test, y_test, w_test)
    f$threshold <- threshold
    structure(f, class=c("cTree", "classification"))
  }
}

fit.cTree <- function(f) {
  yx_train <- data.frame(Y=f$y_train,f$x_train)
  #Setting cp low to ensure trees sufficiently complex
  tree.first <- rpart::rpart(Y~., weights=f$w_train, method="class", data=yx_train, cp=0.001)
  #Chooses tree size with minimal xerror
  bestsize <- tree.first$cptable[which.min(tree.first$cptable[,"xerror"]),"CP"]
  tree.final <- rpart::prune(tree.first, cp = bestsize)  
}

predict.cTree <- function(f, model) {
  temp<-predict(model, data.frame(f$x_test), type = "prob")
  prob_non_poor <- temp[,2]
}


cForest <- function(threshold) {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    f <- fold(x_train, y_train, w_train, x_test, y_test, w_test)
    f$threshold <- threshold
    structure(f, class=c("cforest", "forest", "classification"))
  }
}


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

transform_ys.cbtrees <- function(f) {
  f <- transform_ys.classification(f)
  f$y_train <- as.logical(f$y_train)
  f$y_test <- as.logical(f$y_test)
  f
}

predict.cbtrees <- function(f, model) {
  1 - predict(model, newdata=data.frame(f$x_test), n.trees=f$n.trees, type="response")
}


# Ensemble methods ----------------------------------

Super <- function(SL.library=NULL) {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    f <- structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class=c("super"))
    if (is.null(SL.library)) {
      SL.library <- c()
    }
    f$SL.library <- SL.library
    f
  }
}

fit.super <- function(f) {
  model <- SuperLearner(f$y_train, f$x_train, newX=f$x_test, SL.library=f$SL.library)
}

predict.super <- function(f, model) {
  model$SL.predict
}

# Splines --------------------------------
Spline <- function() {
  function(x_train, y_train, w_train, x_test, y_test, w_test) {
    structure(fold(x_train, y_train, w_train, x_test, y_test, w_test), class="spline")
  }
}

fit.spline <- function(f) {
  polspline::polymars(f$y_train, f$x_train)
}

predict.spline <- function(f, model) {
  predict(model, f$x_test)
}


cv_split <- function(y, x, k=10, inner_k=3, id=NULL, weight=NULL, seed=NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (is.null(weight)) {
    weight <- rep(1,length(y))
  }
  if (is.null(id)) {
    id <- 1:length(y)
  }
  
  outer_assignments <- sample(rep(1:k, length.out=nrow(x)))
  lapply(1:k, function(i) {
    assignments <- outer_assignments != i
    
    x_holdout <- x[!assignments, ]
    y_holdout <- y[!assignments]
    id_holdout <- id[!assignments]
    
    x_train <- x[assignments, ]
    y_train <- y[assignments]
    
    ksplit <- kfold_split(inner_k, y_train, x_train)
    
    list(
      cv=ksplit,
      nocv=kfold_split(1, y_train, x_train),
      x_holdout=x_holdout,
      y_holdout=y_holdout,
      id_holdout=id_holdout)
  })
}


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
  id_sorted <- id[order(assignments)]
  
  splits <- lapply(1:k, function (i) { 
    if (k==1) {
      list(
        x_train=x,
        y_train=y,
        w_train=weight,
        x_test=x,
        y_test=y,
        w_test=weight)
    }
    else {
      list(
        x_train=x[assignments != i, ],
        y_train=y[assignments != i],
        w_train=weight[assignments != i],
        x_test=x[assignments == i, ],
        y_test=y[assignments == i],
        w_test=weight[assignments == i])
    }
  })
  list(splits=splits, assignments=assignments, id_sorted=id_sorted)
}

kfold_add_importance_weights <- function(kfold_splits, threshold, gamma) {
  n <- length(kfold_splits$splits)
  marginal_utility <- function(log_consumption) exp(log_consumption) ^ (- gamma)
  for (i in 1:n) {
    kfold_splits$splits[[i]]$w_train <- abs(marginal_utility(kfold_splits$splits[[i]]$y_train) - marginal_utility(threshold))
  }
  kfold_splits  
}

kfold_fit <- function(kfold_splits, model_class) {
  splits <- kfold_splits$splits
  folds <- lapply(splits, function(s) do.call(model_class, s))
  folds <- lapply(folds, transform_ys)
  fits <- lapply(folds, fit)
  list(folds=folds, fits=fits, assignments=kfold_splits$assignments, id_sorted=kfold_splits$id_sorted)
}

kfold_predict <- function(kfold_fits) {
  folds <- kfold_fits$folds
  fits <- kfold_fits$fits
  assignments <- kfold_fits$assignments
  #Order in df will be ascending in fold number, hence
  assignments <- sort(assignments)
  id_sorted <- kfold_fits$id_sorted[order(assignments)]
  preds <- unlist(mapply(predict, folds, fits, SIMPLIFY=FALSE))
  trues <- unlist(lapply(folds, function(f) f$y_test))
  raws <- unlist(lapply(folds, function(f) f$y_test_raw))
  weight <- unlist(lapply(folds, function(f) f$w_test))
  df <- data.frame(predicted=preds, true=trues, raw=raws, weight=weight, fold=assignments, id=id_sorted)
  df
}

kfold_ <- function(model_class, kfold_splits) {
  kfold_fits <- kfold_fit(kfold_splits, model_class)
  list(pred=kfold_predict(kfold_fits), kfold_fits=kfold_fits)
}

kfold <- function(k, model_class, y, x, id=NULL, weight=NULL, seed=0) {
  kfold_splits <- kfold_split(k, y, x, id, weight, seed)
  kfold_fits <- kfold_fit(kfold_splits, model_class)
  data.frame(kfold_predict(kfold_fits), kfold_splits$id_sorted)
}

ensemble <- function(predictions, holdout_predictions, classification=TRUE) {
  stopifnot(length(predictions) == length(holdout_predictions))
  k <- length(predictions)
  ensemble_list <- lapply(1:k, function(i) {
    res <- predictions[[i]]
    hres <- holdout_predictions[[i]]
    model_names <- intersect(names(res), names(hres))
    res <- res[model_names]
    get_prediction_df <- function(res) {
      res <- lapply(res, function(df) {
        if (all(df$predicted > 2) | classification == TRUE) {
          df[order(df$id), ]
        }
        else {
          NULL
        }
      })
      res <- res[!sapply(res, is.null)]
      trues <- lapply(res, function(r) r$raw)
      sapply(trues, function(t) {
        if (!all(abs(t - trues[[1]]) < .001)) {
          print(t - trues[[1]])
        }
      })
      
      ids <- lapply(res, function(r) r$id)
      sapply(ids, function(id) {
        if (any(id != ids[[1]])) {
          print(paste(id, '!=', ids[[1]]))
        }
      })
      
      cols <- lapply(res, function(r) r$predicted)
      cols$raw <- trues[[1]]
      cols$id <- ids[[1]]
      data.frame(cols)
    }
    print('joining results')
    df <- get_prediction_df(res)
    print('fitting model')
    model  <- lm(raw ~ -1 + ., select(df, -one_of('id')))
    print('joining holdout results')
    df <- get_prediction_df(hres)
    print('making predictions')
    raw_df <- get_prediction_df(res)
    predicted <- predict(model, select(df, -one_of('id', 'raw')))
    print('creating data frame')
    pred <- data.frame(predicted=predicted,
                       true=df$raw,
                       raw=df$raw,
                       id=df$id)
    pred$weight <- 1
    pred$fold <- i
    list(pred=data.frame(pred), fits=model)
  })
  pred_list <- lapply(ensemble_list, function(e) e$pred)
  fits_list <- lapply(ensemble_list, function(e) e$fits)
  list(pred=do.call(rbind, pred_list), fits=fits_list)
}

run_all_models_pca <- function(name, k, y, x, ncomp=20) {
  x <- prcomp(x, retx=TRUE)$x[, 1:ncomp]
  ksplit <- kfold_split(k, y, x, seed=1)
  ksplit_nmm <- kfold_split(k, y, data.frame(x), seed=1)
  run_all_models(name, data.frame(y=y, x), "y", ksplit, ksplit_nmm)
}

run_all_heldout <- function(name, df, target, cv_splits, grouping_variable=NULL) {
  results_no_cv <- lapply(cv_splits, function(single_split) {
    run_all_models(name, df, target, single_split$nocv, grouping_variable=grouping_variable)})
  results <- lapply(cv_splits, function(single_split) {
    run_all_models(name, df, target, single_split$cv, grouping_variable=grouping_variable)})
  run_heldout(name, cv_splits, results, results_no_cv)
}

run_fs_heldout <- function(name, df, target, cv_splits, grouping_variable=NULL, method='stepwise') {
  results <- lapply(cv_splits, function(single_split) {
    run_all_feature_selected(name, df, target, single_split$cv, grouping_variable=grouping_variable, method=method)})
  results_no_cv <- lapply(cv_splits, function(single_split) {
    run_all_feature_selected(name, df, target, single_split$nocv, grouping_variable=grouping_variable, method=method)})
  run_heldout(name, cv_splits, results, results_no_cv)
}


run_weighted_heldout <- function(name, df, target, cv_splits, grouping_variable=NULL) {
  results <- lapply(cv_splits, function(single_split) {
    run_weighted_models(name, df, target, single_split$cv, grouping_variable=grouping_variable)})
  results_no_cv <- lapply(cv_splits, function(single_split) {
    run_weighted_models(name, df, target, single_split$nocv, grouping_variable=grouping_variable)})
  run_heldout(name, cv_splits, results, results_no_cv)
}

run_heldout <- function(name, cv_splits, results, results_no_cv) {
  holdout_predictions <- lapply(1:length(results_no_cv), function(trial_index) {
    cv_split <- cv_splits[[trial_index]]
    lapply(results_no_cv[[trial_index]], function(method_results) {
      kfold_fits <- method_results$kfold_fits
      kfold_fits$assignments <- rep(trial_index, length(cv_split$y_holdout))
      kfold_fits$id_sorted <- cv_split$id_holdout
      for (i in 1:length(kfold_fits$folds)) {
        kfold_fits$folds[[i]]$x_test <- cv_split$x_holdout
        kfold_fits$folds[[i]]$y_test <- cv_split$y_holdout
        kfold_fits$folds[[i]]$w_test <- rep(1, length(cv_split$y_holdout))
      }
      kfold_fits$folds <- lapply(kfold_fits$folds, transform_ys)
      kfold_predict(kfold_fits)
    })
  })

  predictions <- lapply(results, function(res) lapply(res, function(r) r$pred))
  holdout_predictions <- lapply(holdout_predictions, function(hres) {
    is_null <- sapply(hres, is.null)
    hres[!is_null]
  })
  
  e <- ensemble(predictions, holdout_predictions)
  save_ensemble(name, e)
  e_all <- ensemble(predictions, holdout_predictions, classification=FALSE)
  dfs <- lapply(1:length(holdout_predictions), function(i) {
    pred <- holdout_predictions[[i]]
    joined <- join_dfs(pred)
    joined$fold <- i
    joined
  })
  dfs <- do.call(rbind, dfs)
  e$pred$method <- 'ensemble'
  e_all$pred$method <- 'ensemble_all'
  dfs <- rbind(dfs, e$pred)
  dfs <- rbind(dfs, e_all$pred)
  save_models_(name, dfs)
}

run_all_feature_selected <- function(name, df, target, ksplit, ksplit_nmm=NULL, grouping_variable=NULL, method='stepwise') {
  if (method=='lasso') {
    lasso <- kfold_(Lasso(25), ksplit)
    model <- lasso$kfold_fits$fits[[1]]
    col_index <- max(which(colSums(abs(as.matrix(model$beta)) > 0) <=25))
    betas <- model$beta[, col_index]
    columns <- names(betas)[betas > 0]
  }
  if (method == 'stepwise') {
    stepwise <- kfold_fit(ksplit, Stepwise(max_covariates=25, original_columns=colnames(df)))
    model <- stepwise$fits[[1]]
    columns <- model$stepwise_features
  }
  if (method == 'forest') {
    forest <- kfold_fit(ksplit, Forest())
    model <- forest$fits[[1]]
    imp <- model$importance
    columns <- imp[-order(imp[, 1]), ][1:25]
  }
  for (i in 1:length(ksplit$splits)) {
    for (col in columns) {
      if (!(col %in% colnames(ksplit$splits[[i]]$x_train))) {
        print(col)
      }
    }
    ksplit$splits[[i]]$x_train <- ksplit$splits[[i]]$x_train[, columns]
    ksplit$splits[[i]]$x_test <- ksplit$splits[[i]]$x_test[, columns]
  }
  run_all_models(name, df, target, ksplit, ksplit_nmm, grouping_variable)
}

run_all_models <- function(name, df, target, ksplit, ksplit_nmm=NULL, grouping_variable=NULL) {
  if (is.null(ksplit_nmm)) {
    ksplit_nmm <- ksplit
  }
  save_dataset(name, df)
  results <- list()
  
#   print("Running ridge")
#   results$ridge <- kfold_(Ridge(), ksplit)
#   print("Running lasso")
#   results$lasso <- kfold_(Lasso(), ksplit)

  print("Running least squares")
  results$least_squares <- kfold_(LeastSquares(), ksplit)
  
  print("Running randomForest")
  results$forest <- kfold_(Forest(), ksplit_nmm)
#   print("Running tree plus linear")
#   results$tree_plus_linear <- kfold_(TreePlusLinear(), ksplit_nmm)
  print("Running linear plus tree")
  results$linear_plus_forest <- kfold_(LinearPlusForest(), ksplit_nmm)

  prediction_results <- lapply(results, function(res) {res$pred})
  prediction_results$name <- name
  do.call(save_models, prediction_results)
  results
}

run_weighted_models <- function(name, df, target, ksplit, ksplit_nmm=NULL, grouping_variable=NULL) {
  save_dataset(name, df)
  results <- list()
  threshold_40 <- quantile(df[, target], .4, na.rm=TRUE)
  print("Running least squares")
  results$least_squares <- kfold_(LeastSquares(), ksplit)
  
  print("Running logistic")
  results$logistic <- kfold_(Logistic(threshold_40), ksplit)
  
  gamma <- 2
  ksplit <- kfold_add_importance_weights(ksplit, threshold_40, gamma)
  if (is.null(ksplit_nmm)) {
    ksplit_nmm <- ksplit
  }
  
  print("Running weighted least squares")
  results$weighted_least_squares <- kfold_(LeastSquares(), ksplit)
  
  print("Running binary least squares")
  results$weighted_binary_least_squares <- kfold_(BinaryLeastSquares(threshold_40), ksplit)
  
  print("Running weighted logisitc")
  results$weighted_logistic <- kfold_(Logistic(threshold_40), ksplit)
  
  prediction_results <- lapply(results, function(res) {res$pred})
  prediction_results$name <- name
  do.call(save_models, prediction_results)
  results
}




run_all_all_models <- function(name, df, target, ksplit, ksplit_nmm=NULL, grouping_variable=NULL) {
  if (is.null(ksplit_nmm)) {
    ksplit_nmm <- ksplit
  }
  save_dataset(name, df)
  results <- list()
  
  print("Running ridge")
  results$ridge <- kfold_(Ridge(), ksplit)
  print("Running lasso")
  results$lasso <- kfold_(Lasso(), ksplit)
  print("Running lasso 15")
  results$lasso_15 <- kfold_(Lasso(max_covariates=15), ksplit)
  print("Runnin post lasso")
  results$post_lasso <- kfold_(PostLasso(), ksplit)
  print("Runnin post lasso 15")
  results$post_lasso_15 <- kfold_(PostLasso(max_covariates=15), ksplit)

  print("Running least squares")
  results$least_squares <- kfold_(LeastSquares(), ksplit)
  results$least_squares_pc <- kfold_(LeastSquaresPC(), ksplit)
  
  print('Running grouped ridge')
  try(results$grouped_ridge <- kfold_(GroupedRidge(grouping_variable), ksplit_nmm))

  print("Running Quantile")
  results$quantile <- kfold_(QuantileRegression(), ksplit)
  results$quantile_30 <- kfold_(QuantileRegression(tau=0.3), ksplit)
  
  print("Running stepwise")
  try(results$stepwise <- kfold_(Stepwise(300), ksplit))
  print("Running stepwise 15")
  try(results$stepwise_15 <- kfold_(Stepwise(15), ksplit))
  
  print("Running spline")
  results$spline <- kfold_(Spline(), ksplit)
  
  
  print("Running rtree")
  results$rtree <- kfold_(rTree(), ksplit_nmm)
  print("Running randomForest")
  results$forest <- kfold_(Forest(), ksplit_nmm)
  print("Running Boostedtree")
  results$btree <- kfold_(BoostedTrees(), ksplit_nmm)
  results$btree_laplace <- kfold_(BoostedTrees(distribution="laplace"), ksplit_nmm)  
  
  print("Running tree plus linear")
  results$tree_plus_linear <- kfold_(TreePlusLinear(), ksplit_nmm)
  print("Running linear plus tree")
  results$linear_plus_forest <- kfold_(LinearPlusTree(), ksplit_nmm)
  
  print("Running mca")
  try(results$mca_knn <- kfold_(MCA_KNN(ndim=12, k=5), ksplit_nmm))
  print("Running pca")
  try(results$pca_knn <- kfold_(PCA_KNN(ndim=12), ksplit_nmm))
  print("Running pca all")
  try(results$pca_knn_all <- kfold_(PCA_KNN(ndim=12), ksplit))
  
  threshold_40 <- quantile(df[, target], .4, na.rm=TRUE)
  print("Running logistic")
  results$logistic_40 <- kfold_(Logistic(threshold_40), ksplit)
  print("Running logisitic lasso")
  results$logistic_lasso_40 <- kfold_(LogisticLasso(threshold_40), ksplit)
  print(" Running ctree")
  results$ctree_40 <- kfold_(cTree(threshold_40), ksplit_nmm)
  print("Running randomForest")
  results$cforest_40 <- kfold_(cForest(threshold_40), ksplit_nmm)
  print("Running cBoostedtree")
  results$cbtree_40 <- kfold_(cBoostedTrees(threshold_40), ksplit_nmm)
  results$cbtree_adaboost_40 <- kfold_(cBoostedTrees(threshold_40, distribution="adaboost"), ksplit_nmm)
  results$cbtree_huberized_40 <- kfold_(cBoostedTrees(threshold_40, distribution="huberized"), ksplit_nmm)  
  
  threshold_30 <- quantile(df[, target], .3, na.rm=TRUE)
  print("Running logistic")
  results$logistic_30 <- kfold_(Logistic(threshold_30), ksplit)
  print("Running logisitic lasso")
  results$logistic_lasso_30 <- kfold_(LogisticLasso(threshold_30), ksplit)
  print(" Running ctree")
  results$ctree_30 <- kfold_(cTree(threshold_30), ksplit_nmm)
  print("Running randomForest")
  results$cforest_30 <- kfold_(cForest(threshold_30), ksplit)
  print("Running cBoostedtree")
  results$cbtree_30 <- kfold_(cBoostedTrees(threshold_30), ksplit)
  results$cbtree_adaboost_30 <- kfold_(cBoostedTrees(threshold_30, distribution="adaboost"), ksplit)  
  results$cbtree_huberized_30 <- kfold_(cBoostedTrees(threshold_30, distribution="huberized"), ksplit)  

  prediction_results <- lapply(results, function(res) {res$pred})
  prediction_results$name <- name
  do.call(save_models, prediction_results)
  results
}