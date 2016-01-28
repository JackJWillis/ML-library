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

split_test_train <- function(survey_df, k=K) {
  assignments <- get_assignments(survey_df, k)
  folds <- lapply(1:k, function(i) {
    is_test <- assignments == i
    fold <- construct_fold(survey_df, is_test)
    fold$fold_number <- i
    fold
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
  df
}


test_method_on_splits <- function(method, folds) {
  method_name <- names(method)
  parallel <- (method_name != 'tpo') # RWeka does not work with multicore
  if (parallel) {
    mclapply(
      folds,
      function(fold) test_one(method, fold),
      mc.cores=detectCores())
  } else {
    lapply(
      folds,
      function(fold) test_one(method, fold))
  }
}


test_all <- function(survey_df, method_list=NULL, k=K, seed=1) {
  if (is.null(method_list)) {
    method_list <- METHOD_LIST
    if (ncol(survey_df) > 26) {
      method_list <- c(method_list, METHOD_25_LIST)
    }
  }
  set.seed(seed)
  folds <- split_test_train(survey_df, k)
  method_results <- purrr::flatmap(names(method_list), function(method_name) {
    print(method_name)
    method <- method_list[method_name]
    test_method_on_splits(method, folds)
  })
  bind_rows(method_results)
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


ols <- function(fold) {
  model <- fit_ols(fold$train)
  test <- knockout_new_categories(fold$test, fold$train)
  test <- impute_all(test)
  predict(model, test)
}


fit_forest <- function(df, ntree=NULL, nodesize=NULL) {
  if (nrow(df) < 2000) ntree <- 200 else ntree <- 50
  if(is.null(nodesize)) nodesize <- ceiling(.005 * nrow(df))
  y <- df[, TARGET_VARIABLE]
  x <- select(df, -one_of(TARGET_VARIABLE, WEIGHT_VARIABLE))
  randomForest::randomForest(x=x, y=y, ntree=ntree, nodesize=nodesize)
}


forest <- function(fold) {
  model <- fit_forest(fold$train)
  predict(model, fold$test)
}


ols_plus_forest <- function(fold) {
  linear <- fit_ols(fold$train)
  res_df <- fold$train
  res_df[, TARGET_VARIABLE] <- residuals(linear)
  nonlinear <- fit_forest(res_df)
  
  test <- knockout_new_categories(fold$test, fold$train)
  test <- impute_all(test)
  predict(linear, test) + predict(nonlinear, fold$test)
}


tree <- function(fold) {
  model <- rpart::rpart(as.formula(FMLA_STR), fold$train)
  predict(model, fold$test)
}


ols_plus_tree <- function(fold) {
  linear <- fit_ols(fold$train)
  res_df <- fold$train
  res_df[, TARGET_VARIABLE] <- residuals(linear)
  nonlinear <- rpart::rpart(as.formula(FMLA_STR), res_df)
  
  predict(linear, fold$test) + predict(nonlinear, fold$test)
}


tree_plus_ols <- function(fold) {
  model <- RWeka::M5P(as.formula(FMLA_STR), fold$train)
  predict(model, fold$test)
}


ols_forest_ensemble <- function(fold) {
  train <- fold$train
  ensemble_folds <- split_test_train(train, k=4)
  
  ols_predictions <- flatmap(ensemble_folds, ols)
  forest_predictions <- flatmap(ensemble_folds, forest)
  opf_predictions <- flatmap(ensemble_folds, ols_plus_forest)
  true <- flatmap(ensemble_folds, ~.$test[, TARGET_VARIABLE])
  
  ensemble_df <- data.frame(
    true=true,
    ols=ols_predictions,
    forest=forest_predictions,
    opf=opf_predictions)
  ensemble <- lm(true ~ ., data=ensemble_df)
  
  ols_test_predictions <- ols(fold)
  forest_test_predictions <- forest(fold)
  opf_test_predictions <- ols_plus_forest(fold)
  test_predictions_df <- data.frame(
    ols=ols_test_predictions, 
    forest=forest_test_predictions,
    opf=opf_test_predictions)
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


elastic_net <- function(fold) {
  train <- get_weights(fold$train)
  x <- model.matrix(as.formula(FMLA_STR), train$data)
  y <- train$data[, TARGET_VARIABLE]
  cv.model <- glmnet::cv.glmnet(x, y, weights=train$weight_vector, alpha=0.5)
  model <- glmnet::glmnet(x, y, alpha=0.5)
  
  test <- knockout_new_categories(fold$test, fold$train)
  test <- impute_all(test)
  test <- get_weights(test)
  test_x <- model.matrix(as.formula(FMLA_STR), test$data)
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


METHOD_25_LIST <- list(
  ols_25=ols_25,
  ensemble_25=ensemble_25
)

METHOD_LIST <- list(
  ols=ols,
  enet=elastic_net,
  forest=forest,
  opf=ols_plus_forest,
#   opt=ols_plus_tree,
#   tpo=tree_plus_ols,
  ensemble=ols_forest_ensemble
  # enet_ix=elastic_net_ix,
)

SCALE_METHODS <- list(
  ols=ols,
  tree=tree
)
