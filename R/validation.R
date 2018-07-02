#' @export

TARGET_VARIABLE <- 'yyyyy'
WEIGHT_VARIABLE <- 'weight_weight_weight'
FMLA_STR <- paste(TARGET_VARIABLE, "~ .")
K <- 5 

#### Training/Testing ######

get_assignments <- function(survey_df, k) {
  sample(rep(1:k, length.out=nrow(survey_df)))
}


construct_fold <- function(survey_df, is_test) {
  test <- survey_df[is_test, ]
  train <- survey_df[!is_test, ]
  list(test=test, train=train)
}

split_test_train <- function(survey_df, k=K, test_fraction=NULL) {
  if (is.null(test_fraction)) {
    assignments <- get_assignments(survey_df, k)
    folds <- lapply(1:k, function(i) {
      is_test <- assignments == i
      fold <- construct_fold(survey_df, is_test)
      fold$fold_number <- i
      fold
    })
  } else {
    is_test <- as.logical(rbinom(nrow(survey_df), 1, test_fraction))
    fold <- construct_fold(survey_df, is_test)
    fold$fold_number <- 1
    folds <- list(fold)
  }
  folds
}


test_one <- function(method, fold) {
  stopifnot(length(method) == 1)
  method_name <- names(method)
  method <- method[[1]]
  predictions <- method(fold)
  df <- data.frame(
    true=fold$test[, TARGET_VARIABLE],
    predicted=predictions,
    fold=fold$fold_number)
  if (!is.null(method_name)) {
    df$method <- method_name
  }
  df
}


test_method_on_splits <- function(method, folds) {
  method_name <- names(method)
  lapply(
    folds,
    function(fold) test_one(method, fold))
}


test_all_named <- function(name, survey_df, method_list=METHOD_LIST, k=K, test_fraction=NULL, seed=1) {
  named_method_list <- lapply(method_list, function(method) named_method(name, method))
  test_all(survey_df, named_method_list, k, test_fraction, seed)
}


test_all <- function(survey_df, method_list=NULL, k=K, test_fraction=NULL, seed=1) {
  if (is.null(method_list)) {
    method_list <- METHOD_LIST
    if (ncol(survey_df) > 26) {
      method_list <- c(method_list, METHOD_25_LIST)
    }
  }
  set.seed(seed)
  folds <- split_test_train(survey_df, k, test_fraction)
  method_results <- lapply(
    names(method_list), 
    function(method_name) {
      print(method_name)
      method <- method_list[method_name]
      test_method_on_splits(method, folds)
    }
  )
  bind_rows(flatten(method_results))
}


##### Output #####
DEFAULT_THRESHOLDS <- seq(0.1, 0.4, .1)

add_threshold <- function(output, threshold) {
  consumption_cutoff <- quantile(output$true, threshold)
  output <- output[rep(1:nrow(output), each=length(threshold)), ]
  output$threshold <- as.integer(threshold * 100)
  output$consumption_cutoff <- consumption_cutoff
  output
}


order_by_predicted <- function(output) {
  output %>% 
    arrange(predicted) %>%
    mutate(pct_targeted=row_number() / n())
}


reach_by_pct_targeted <- function(output, threshold=DEFAULT_THRESHOLDS, fold=FALSE) {
  with_threshold <- add_threshold(output, threshold)
  if (fold) {
    grouped <- group_by(with_threshold, method, fold, threshold)
  } else {
    grouped <- group_by(with_threshold, method, threshold)
  }
  grouped %>%
    order_by_predicted() %>%
    mutate(tp=true < consumption_cutoff) %>%
    mutate(value=cumsum(tp) / n())
}


value_at_pct <- function(stat_by_pct) {
  filter(stat_by_pct, pct_targeted <= threshold / 100) %>%
    summarize(value=last(value))
}


budget_change <- function(stat_by_pct, base='ols') {
  # get a dataframe of just the base stat
  base_stat <- value_at_pct(stat_by_pct) %>%
    filter(method == base) %>%
    ungroup() %>%
    select(threshold, base_value=value) 
  # add the base stat as a column to original df by merging
  merged <- merge(stat_by_pct, base_stat, by='threshold') %>%
    group_by(method, threshold) %>%
    arrange(pct_targeted)
  # get budget change by method and threshold
  bc <- merged %>%
    filter(value <= base_value) %>%
    summarize(pct_targeted=last(pct_targeted), base_pct=last(threshold) / 100) %>%
    mutate(value=(pct_targeted - base_pct) / base_pct) %>%
    select(method, threshold, value)
  # reshape so that each row is keyed by method
  # with results for each threshold as columns
  bc %>%
    reshape::cast(method ~ threshold)
}


plot_stat <- function(stat_by_pct) {
  ggplot(reach, aes(x=pct_targeted, y=value, color=method)) +
    geom_line() +
    facet_wrap(~ threshold)
}


table_stat <- function(stat_by_pct) {
  stat_df <- value_at_pct(stat_by_pct)
  reshape::cast(stat_df, method ~ threshold)
}


##### Models #####


get_weights <- function(df) {
  if (WEIGHT_VARIABLE %in% colnames(df)) {
    weight_vector <- df[, WEIGHT_VARIABLE]
  } else {
    weight_vector <- rep(1, nrow(df))
  }
  df <- select(df, -one_of(WEIGHT_VARIABLE))
  list(data=df, weight_vector=weight_vector)
}


fit_ols <- function(df) {
  wdf <- get_weights(df)
  df <- droplevels(wdf$data)
  df <- purrr::keep(df, ~length(na.omit(unique(.))) > 1)
  weight_vector <- wdf$weight_vector
  fmla <- as.formula(FMLA_STR)
  lm(fmla, data=df, weights=weight_vector)
}


ols <- function(fold, ...) {
  model <- fit_ols(fold$train)
  test <- knockout_new_categories(fold$test, fold$train)
  test <- impute_all(test)
  predict(model, test)
}


forest_h2o <- function(fold, mtries=-1, ntrees=1000, min_nodesize=35, max_depth=1000, ...) {
  h2o::h2o.init(nthreads=-1, max_mem_size='5g')
  train.hex <- h2o::as.h2o(fold$train, destination_frame="train.hex")
  y <- which(colnames(fold$train) == TARGET_VARIABLE)
  x <- (1:ncol(fold$train))[-y]
  if (!(mtries == -1)) {
    mtries <- ceiling(ncol(fold$train) * mtries)
  }
  if (abs(min_nodesize) < 1) {
    min_nodesize <- ceiling(nrow(fold$train) * min_nodesize)
  }
  model <- h2o::h2o.randomForest(
      y=y,
      x=x,
      training_frame=train.hex,
      mtries=mtries,
      min_rows=min_nodesize,
      max_depth=max_depth,
      ntrees=ntrees)
  test.hex <- h2o::as.h2o(fold$test, destination_frame="test.hex")
  predictions <- as.vector(h2o::h2o.predict(model, test.hex))
  h2o::h2o.rm(train.hex)
  h2o::h2o.rm(test.hex)
  h2o::h2o.rm(model)
  predictions
}


fit_forest <- function(df, ntree=NULL, nodesize=NULL) {
  if (nrow(df) < 2000) ntree <- 200 else ntree <- 50
  if(is.null(nodesize)) nodesize <- ceiling(.005 * nrow(df))
  y <- df[, TARGET_VARIABLE]
  x <- select(df, -one_of(TARGET_VARIABLE, WEIGHT_VARIABLE))
  randomForest::randomForest(x=x, y=y, ntree=ntree, nodesize=nodesize)
}



forest <- function(fold, ...) {
  model <- fit_forest(fold$train)
  if (is.factor(fold$test[, TARGET_VARIABLE])) {
    type <- 'prob'
    predict(model, fold$test, type=type)[, 1]
  } else { 
    type <- 'response'
    predict(model, fold$test, type=type)
  }
}


binary_forest_maker <- function(threshold) {
  function(fold) {
    train_y <- fold$train[, TARGET_VARIABLE]
    threshold_cons <- quantile(train_y, threshold)
    fold$train[, TARGET_VARIABLE] <- as.factor(train_y < threshold_cons)
    fold$test[, TARGET_VARIABLE] <- as.factor(fold$test[, TARGET_VARIABLE] < threshold_cons)
    forest(fold)
  }
}

named_method <- function(name, method) {
  function(fold) method(fold, name=name)
}

tuned_forest <- function(fold, name=NULL) {
  config <- if (!is.null(name)) read_config(name) else list()
  best_parameters <- config$tuned_forest
  if (is.null(best_parameters)) {
    df <- fold$train
    new_folds <- split_test_train(df, test_fraction=0.2)
    nodesizes <- c(.0005, .001, .0025, .005, .0075, .01) 
    mtries <- c(1/6, 2/6, 3/6, 4/6, -1)
    parameters <- expand.grid(ns=nodesizes, mt=mtries)
    print(parameters)
    forest_predictions <- mapply(
      function(nodesize, mtry) {
        print(paste(nodesize, mtry))
        flatten_dbl(lapply(new_folds, function(new_fold) {
          forest_h2o(new_fold, mtries=mtry, ntrees=250, min_nodesize=nodesize)
        }))
      },
      parameters$ns,
      parameters$mt, 
      SIMPLIFY=FALSE
    )
    true <- flatten_dbl(map(new_folds, ~.$test[, TARGET_VARIABLE]))
    losses <- sapply(
      forest_predictions,
      function(predicted) {
        mean((predicted - true) ^ 2)
      }
    )
    best_parameters <- parameters[which.min(losses), ]
    print(best_parameters)
    config$tuned_forest <- best_parameters
    save_config(name, config)
  }
  forest_h2o(fold, mtries=best_parameters$mt, ntrees=2500, min_nodesize=best_parameters$ns)
}

ols_plus_forest <- function(fold, tuned=FALSE, name=NULL) {
  linear <- fit_ols(fold$train)
  res_df <- fold$train
  res_df[, TARGET_VARIABLE] <- residuals(linear)
  
  res_fold <- list(train=res_df, test=fold$test)
  if (tuned) {
    nonlinear_predictions <- tuned_forest(res_fold, name)
  } else {
    nonlinear_predictions <- forest(res_fold)
  }
  
  test <- knockout_new_categories(fold$test, fold$train)
  test <- impute_all(test)
  predict(linear, test) + nonlinear_predictions
}

ols_plus_tuned_forest <- function(fold, name=NULL) {
  ols_plus_forest(fold, tuned=TRUE, name=name)
}


tree <- function(fold) {
  wdf <- get_weights(fold$train)
  model <- rpart::rpart(as.formula(FMLA_STR), wdf$data, weights=wdf$weight_vector, cp=.0001)
  cp <- model$cptable[which.min(model$cptable[, "xerror"]), "CP"]
  pruned <- rpart::prune(model, cp=cp)
  predict(pruned, newdata=fold$test)
}


ols_plus_tree <- function(fold) {
  linear <- fit_ols(fold$train)
  res_df <- fold$train
  res_df[, TARGET_VARIABLE] <- residuals(linear)
  nonlinear <- rpart::rpart(as.formula(FMLA_STR), res_df)
  
  predict(linear, fold$test) + predict(nonlinear, fold$test)
}


tree_plus_ols <- function(fold, ...) {
  model <- RWeka::M5P(as.formula(FMLA_STR), fold$train)
  predict(model, fold$test)
}


boosted_tree <- function(fold) {
  wdf <- get_weights(fold$train)
  fmla <- as.formula(FMLA_STR)
  model <- gbm::gbm(fmla, data=wdf$data, weights=wdf$weight_vector, interaction.depth=4)
  predict(model, newdata=fold$test)
}

tuned_boosted_tree <- function(fold) {
  df2 <- fold$train
  new_fold <- split_test_train(df2, test_fraction=.2)[[1]]
  wdf <- get_weights(new_fold$train)
  fmla <- as.formula(FMLA_STR)
  
  shrinkage <- c(1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6)
  interaction_depths <- seq(2, 20, by=4)
  n_minobsinnode <- seq(2, 20, by=4)
  parameters <- expand.grid(depths=interaction_depths, nobs=n_minobsinnode, shrink=shrinkage)
  models <- mcmapply(
    function(depth, nob, shrink) {
      gbm::gbm(
        fmla,
        data=wdf$data,
        weights=wdf$weight_vector,
        interaction.depth=depth,
        n.minobsinnode=nob,
        shrinkage=shrink)
    },
    parameters$depths,
    parameters$nobs,
    parameters$shrink,
    SIMPLIFY=FALSE,
    mc.cores=4
  )
  test_test <- new_fold$test
  losses <- purrr::map_dbl(models, ~mean((predict(.x, test_test, n.trees=100)-test_test[, TARGET_VARIABLE])^2))
  best_parameters <- parameters[which.min(losses), ]
  
  wdf <- get_weights(fold$train)
  model <- gbm::gbm(fmla, data=wdf$data, weights=wdf$weight_vector, interaction.depth=best_parameters$depths, n.minobsinnode=best_parameters$nobs, shrinkage=best_parameters$shrink)
  predict(model, newdata=fold$test, n.trees=100)
}

ols_forest_ensemble <- function(fold, name=NULL) {
  train <- fold$train
  ensemble_folds <- split_test_train(train, k=5)
  
  ols_predictions <- flatten_dbl(map(ensemble_folds, ols))
  forest_predictions <- flatten_dbl(map(ensemble_folds, ~tuned_forest(., name=name)))
  opf_predictions <- flatten_dbl(map(ensemble_folds, ~ols_plus_tuned_forest(., name=name)))
  tpo_predictions <- flatten_dbl(map(ensemble_folds, ~tree_plus_ols(.)))
  true <- flatten_dbl(map(ensemble_folds, ~.$test[, TARGET_VARIABLE]))
  
  ensemble_df <- data.frame(
    true=true,
    ols=ols_predictions,
    forest=forest_predictions,
    opf=opf_predictions,
    tpo=tpo_predictions)
  ensemble <- lm(true ~ ., data=ensemble_df)
  
  ols_test_predictions <- ols(fold)
  forest_test_predictions <- tuned_forest(fold, name=name)
  opf_test_predictions <- ols_plus_tuned_forest(fold, name=name)
  tpo_test_predictions <- tree_plus_ols(fold)
  test_predictions_df <- data.frame(
    ols=ols_test_predictions, 
    forest=forest_test_predictions,
    opf=opf_test_predictions,
    tpo=tpo_test_predictions)
  predict(ensemble, test_predictions_df)
}

fit_lasso <- function(train) {
  x <- model.matrix(as.formula(FMLA_STR), train$data)
  y <- train$data[, TARGET_VARIABLE]
  model <- glmnet::glmnet(x, y, alpha=1)
}

get_original_columns <- function(mm_columns, original_columns) {
  idx <- sapply(original_columns, function(original_column) {
    any(grepl(original_column, mm_columns))
  })
  original_columns[idx]
}

get_25_original_columns <- function(lasso_model, original_columns) {
  i <- 1
  flist <- NULL
  mm_columns <- rownames(lasso_model$beta)[nonzeroCoef(lasso_model$beta)]
  while (length(flist) < 25) {
    flist <- get_original_columns(mm_columns[1:i], original_columns)
    i <- i + 1
  }
  flist
}

ols_25 <- function(fold) {
  train <- get_weights(fold$train)
  m <- fit_lasso(train)
  df <- train$data
  cols <- get_25_original_columns(m, colnames(df))
  cut_df <- df[, cols]
  cut_df[, TARGET_VARIABLE] <- df[, TARGET_VARIABLE]
  cut_df[, WEIGHT_VARIABLE] <- train$weight_vector
  model <- fit_ols(cut_df)
  test <- knockout_new_categories(fold$test, fold$train)
  test <- impute_all(test)
  predict(model, test)
}

ensemble_25 <- function(fold) {
  train <- get_weights(fold$train)
  m <- fit_lasso(train)
  df <- train$data
  cols <- get_25_original_columns(m, colnames(df))
  train <- df[, cols]
  train[, TARGET_VARIABLE] <- df[, TARGET_VARIABLE]
  train[, WEIGHT_VARIABLE] <- train$weight_vector
  new_fold <- list(train=train, test=fold$test)
  ols_forest_ensemble(new_fold)
}


elastic_net <- function(fold, ...) {
  penalized_regression(fold, alpha=0.5)
}

lasso <- function(fold) {
  penalized_regression(fold, alpha=1)
}

ridge <- function(fold) {
  penalized_regression(fold, alpha=0)
}

penalized_regression <- function(fold, alpha) {
  train <- get_weights(fold$train)
  x <- model.matrix(as.formula(FMLA_STR), train$data)
  y <- train$data[, TARGET_VARIABLE]
  cv.model <- glmnet::cv.glmnet(x, y, weights=train$weight_vector, alpha=alpha)
  model <- glmnet::glmnet(x, y, alpha=alpha)
  
  test <- knockout_new_categories(fold$test, fold$train)
  test <- impute_all(test)
  test <- get_weights(test)
  test_x <- model.matrix(as.formula(FMLA_STR), test$data)[, 1:ncol(x)]
  as.numeric(predict(model, test_x, s=cv.model$lambda.min))
}

elastic_net_ix <- function(fold) {
  train <- get_weights(fold$train)
  fmla_ix <- paste(FMLA_STR, '.*.', sep='+')
  x <- model.matrix(as.formula(fmla_ix), train$data)
  y <- train$data[, TARGET_VARIABLE]
  cv.model <- glmnet::cv.glmnet(x, y, weights=train$weight_vector, alpha=0.5)
  model <- glmnet::glmnet(x, y, alpha=0.5)
  
  test <- knockout_new_categories(fold$test, fold$train)
  test <- impute_all(test)
  test <- get_weights(test)
  test_x <- model.matrix(as.formula(fmla_ix), test$data)
  as.numeric(predict(model, test_x, s=cv.model$lambda.min))
}

lasso_ix <- function(fold) {
  train <- get_weights(fold$train)
  fmla_ix <- paste(FMLA_STR, ' .*.', sep='+')
  x <- model.matrix(as.formula(fmla_ix), train$data)
  y <- train$data[, TARGET_VARIABLE]
  cv.model <- glmnet::cv.glmnet(x, y, weights=train$weight_vector, alpha=0.5)
  model <- glmnet::glmnet(x, y, alpha=0.5)
  
  test <- knockout_new_categories(fold$test, fold$train)
  test <- impute_all(test)
  test <- get_weights(test)
  test_x <- model.matrix(as.formula(fmla_ix), test$data)
  as.numeric(predict(model, test_x, s=cv.model$lambda.min))
}


knn <- function(fold) {
  wdf <- get_weights(fold$train)
  fmla <- as.formula(FMLA_STR)
  model <- train.kknn(fmla, wdf$data)
  predict(model, newdata=fold$test)
}

svm <- function(fold) {
  wdf <- get_weights(fold$train)
  fmla <- as.formula(FMLA_STR)
  model <- e1071::best.tune('svm', fmla, data=wdf$data)
  predict(model, fold$test)
}

gaussian_process <- function(fold) {
  wdf <- get_weights(fold$train)
  fmla <- as.formula(FMLA_STR)
  model <- kernlab::gausspr(fmla, wdf$data)
  as.vector(kernlab::predict(model, fold$test))
}

kernel_quantile <- function(fold) {
  wdf <- get_weights(fold$train)
  fmla <- as.formula(FMLA_STR)
  model <- kernlab::kqr(fmla, wdf$data)
  as.vector(kernlab::predict(model, fold$test))
  
}

METHOD_25_LIST <- list(
  ols_25=ols_25,
  ensemble_25=ensemble_25
)

METHOD_LIST <- list(
  ols=ols,
  enet=elastic_net,
  forest=forest,
  tuned_forest=tuned_forest,
  forest_h2o=forest_h2o,
  opf=ols_plus_tuned_forest,
  # opt=ols_plus_tree
  tpo=tree_plus_ols,
  ensemble=ols_forest_ensemble
  # enet_ix=elastic_net_ix,
)

  

SENDHIL_METHODS <- list(
  ols=ols,
  lasso=lasso,
  ridge=ridge,
  enet=elastic_net,
  tree=tree,
  # knn=knn,
  # svm=svm,
  # gp=gaussian_process,
  regression_tree=tree_plus_ols,
  tuned_forest=tuned_forest,
  forest=forest,
  boosted_tree=tuned_boosted_tree,
  ols_plus_forest=ols_plus_tuned_forest
  # ensemble=ols_forest_ensemble
# #   ensemble=ensemble_sendhil
)

SCALE_METHODS <- list(
  ols=ols,
  tree=tree
)
