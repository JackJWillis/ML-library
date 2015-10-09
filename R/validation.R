TARGET_VARIABLE <- 'yyyyy'
FORMULA <- as.formula(paste(TARGET_VARIABLE, "~ ."))
K <- 8 # Because I have 4 cores :/

#### Training/Testing ######

get_assignments <- function(survey_df, k) {
  sample(rep(1:k, length.out=nrow(survey_df)))
}


split_test_train <- function(survey_df, k=K) {
  assignments <- get_assignments(survey_df, k)
  folds <- lapply(1:k, function(i) {
    is_test <- assignments == i
    test <- survey_df[is_test, ]
    train <- survey_df[!is_test, ]
    list(test=test, train=train, fold_number=i)
  })
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
  
  # we want to calculate poverty thresholds from the training data
  # (not the testing data)
  # so we do that here
  quantiles <- quantile(
    fold$train[, TARGET_VARIABLE],
    seq(0., 0.5, .1))
  for (q in names(quantiles)) {
    df[, q] <- quantiles[q]
  }
  df
}


test_method_on_splits <- function(method, folds) {
  mclapply(
    folds,
    function(fold) test_one(method, fold),
    mc.cores=detectCores())
}


test_all <- function(survey_df, method_list=METHOD_LIST, k=K) {
  folds <- split_test_train(survey_df, k)
  method_results <- purrr::flatmap(names(method_list), function(method_name) {
    print(method_name)
    method <- method_list[method_name]
    predictions <- test_method_on_splits(method, folds)
  })
  purrr::reduce(method_results, rbind)
}

##### Output #####

order_by_pct_targeted <- function(output) {
  grouped <- group_by(output, method)
  grouped %>% 
    arrange(prediction) %>%
    mutate(pct_targeted=row_number() / n())
}

reach_by_pct_targeted <- function(output) {
  threshold_columns <- names(output)[grepl('%', names(output))]
  true_lt_thresh <- lapply(threshold_columns, function(thresh) paste('true <', thresh))
  reach <- lapply(threshold_columns, function(thresh) paste('cumsum(', thresh, ')'))
  ordered <- order_by_pct_targeted
  ordered %>%
    mutate_(.dots=true_lt_thresh) %>%
    summarize_(.dots=reach)
}

##### Models #####

ols <- function(fold) {
  model <- lm(FORMULA, data=fold$train)
  predict(model, fold$test)
}


fit_forest <- function(df, ntree=100) {
  y <- df[, TARGET_VARIABLE]
  x <- select(df, -one_of(TARGET_VARIABLE))
  randomForest::randomForest(x=x, y=y, ntree=ntree)
}

forest <- function(fold) {
  model <- fit_forest(fold$train)
  predict(model, fold$test)
}


ols_plus_forest <- function(fold) {
  linear <- lm(FORMULA, data=fold$train)
  res_df <- fold$train
  res_df[, TARGET_VARIABLE] <- residuals(linear)
  nonlinear <- fit_forest(fold$train)
  
  predict(linear, fold$test) + predict(nonlinear, fold$test)
}


ols_forest_ensemble <- function(fold) {
  holdout_fraction <- 0.2
  assignments <- as.logical(rbinom(nrow(fold$train), 1, 1 - holdout_fraction))
  train <- fold$train[assignments, ]
  train_holdout <- fold$train[!assignments, ]
  
  ols_model <- lm(FORMULA, train)
  ols_predictions <- predict(ols_model, train_holdout)
  forest_model <- fit_forest(train)
  forest_predictions <- predict(forest_model, train_holdout)
  ensemble_df <- data.frame(
    true=train_holdout[, TARGET_VARIABLE],
    ols=ols_predictions,
    forest=forest_predictions)
  ensemble <- lm(true ~ ols + forest, data=ensemble_df)
  
  ols_test_predictions <- predict(ols_model, fold$test)
  forest_test_predictions <- predict(forest_model, fold$test)
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