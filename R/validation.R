TARGET_VARIABLE <- 'yyyyy'
FORMULA <- as.formula(paste(TRAIN_VARIABLE, "~ ."))
K <- 8 # Because I have 4 cores :/

#### Training/Testing ######

get_assignments <- function(survey_df, k) {
  sample(rep(1:k, length.out=nrow(survey_df)))
}


split_test_train <- function(survey_df, k=K) {
  assignments <- get_assignments(survey_df, k)
  splits <- lapply(1:k, function(i)) {
    is_test <- assignments == i
    test <- survey_df[is_test, ]
    train <- survey_df[!is_test, ]
    list(test=test, train=train, fold=i)
  }
  splits
}


test_one(method, split) {
  predictions <- method(split)
  df <- data.frame(
    true=split$test[, TARGET_VARIABLE],
    predicted=predictions,
    fold=split$i)
  if (!is.null(names(method))) {
    df$method <- names(method)
  }
  
  # we want to calculate poverty thresholds from the training data
  # (not the testing data)
  # so we do that here
  quantiles <- quantile(
    split$train[, TARGET_VARIABLES],
    seq(0., 0.5, .1)
  names(quantiles) <- paste(
    'threshold',
    names(quantiles),
    sep='_')
  df[, names(quantiles)] <- quantiles
  df
}


test_method_on_splits(method, splits) {
  mclapply(
    splits,
    function(split) test_one(method, split))
}


test_all(survey_df, method_list=method_list, k=K) {
  splits <- split_test_train(survey_df, k)
  method_results <- lapply(names(method_list)) {
    method <- method_list[method_name]
    predictions <- test_method_on_splits(method, splits)
  }
  method_results <- unlist(method_results)
  do.call('rbind', method_results)
}

##### Output #####

order_by_pct_targeted <- function(output) {
  grouped <- group_by(output, method)
  grouped %>% 
    arrange(prediction) %>%
    mutate(pct_targeted=row_number() / n())
}

reach_by_pct_targeted <- function(output) {
  threshold_columns <- names(output)[grepl('threshold', names(ouput))]
  true_lt_thresh <- lapply(threshold_columns, function(thresh) paste('true <', thresh))
  reach <- lapply(threshold_columns, function(thresh) paste('cumsum(', thresh, ')'))
  ordered <- order_by_pct_targeted
  ordered %>%
    mutate(.dots=true_lt_thresh) %>%
    summarize(.dots=reach)
}

##### Models #####

ols <- function(split) {
  model <- lm(FORMULA, data=split$train)
  predict(model, split$test)
}


fit_forest <- function(df, ntree=100) {
  randomForest::randomForest(FORMULA, data=df, ntree=ntree}
}

forest <- function(split) {
  model <- fit_forest(split$train)
  predict(model, split$test)
}


ols_plus_forest <- function(split) {
  linear <- lm(FORMULA, data=split$train)
  res_df <- dplyr::mutate(split$train, .dots(setNames(TARGET_VARIABLE, residuals(linear))))
  nonlinear <- fit_forest(split$train)
  
  predict(linear, split$test) + predict(nonlinear, split$test)
}


ols_forest_ensemble <- function(split) {
  holdout_fraction <- 0.2
  assignments <- as.logical(rbinom(nrow(split$train), 1, 1 - holdout_fraction))
  train <- split$train[assignments, ]
  train_holdout <- split$train[!assignments, ]
  train <- list(train=train, test=train_holdout)
  
  ols_model <- lm(FORMULA, train)
  ols_predictions <- predict(ols_model, train_holdout)
  forest_model <- fit_forest(train)
  forest_predictions <- predict(forest_model, train_holdout)
  ensemble_df <- data.frame(
    true=train_holdout[, TARGET_VARIABLE],
    ols=ols_predictions,
    forest=forest_predictions)
  ensemble <- lm(true ~ ols + forest, data=ensemble_df)
  
  ols_test_predictions <- predict(ols_model, test)
  forest_test_predictions <- predict(forest_model, test)
  test_predictions_df <- data.frame(
    ols=ols_test_predictions, 
    forest=forest_test_predictions)
  predict(ensemble, test_predictions_df)
}

METHOD_LIST <- list(
  ols=ols,
  forest=forest,
  opf=ols_plus_forest,
  ensemble=ols_forest_ensemble)