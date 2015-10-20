scale_n <- function(survey_df, method_list=SCALE_METHODS, holdout_fraction=.2, steps=100) {
  method_list <- map(names(method_list), ~set_names(method_list[.x], .x)) #HACK
  is_test <- as.logical(rbinom(nrow(survey_df), 1, holdout_fraction))
  split <- construct_fold(survey_df, is_test)
  train <- split$train
  test <- split$test
  trial_sizes <- ceiling(seq(ncol(train)+1, nrow(train), length=steps))
  
  apply_method_to_size <- function(method, trial_size) {
    train_subset <- train[1:trial_size, ]
    non_degenerate <- as.list(map(train_subset, ~length(unique(.x)))) > 1
    train_subset <- train_subset[, non_degenerate]
    fold <- list(train=train_subset, test=test, fold_number=trial_size)
    test_one(method, fold)
  }
  
  results <- mapply(
    apply_method_to_size,
    method_list,
    trial_sizes,
    SIMPLIFY=FALSE)
    # mc.cores=detectCores())
  results <- reduce(results, rbind)
}