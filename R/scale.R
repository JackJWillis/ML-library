scale_n <- function(survey_df, method_list=SCALE_METHODS, holdout_fraction=.2, steps=100) {
#   survey_mm <- as.data.frame(model.matrix(FORMULA, survey_df))
#   survey_mm[, TARGET_VARIABLE] <- survey_df[, TARGET_VARIABLE]
  method_list <- map(names(method_list), ~set_names(method_list[.x], .x)) #HACK
  is_test <- as.logical(rbinom(nrow(survey_df), 1, holdout_fraction))
  split <- construct_fold(survey_df, is_test)
  train <- split$train
  test <- split$test
  queue <- sample(1:nrow(train))
  trial_sizes <- ceiling(seq(ncol(train)+1, nrow(train), length=steps))
  
  apply_method_to_size <- function(method, trial_size) {
    train_subset <- train[queue[1:trial_size], ]
    non_degenerate <- as.list(map(train_subset, ~length(unique(.x)))) > 1
    train_subset <- train_subset[, non_degenerate]
    fold <- list(train=train_subset, test=test, fold_number=trial_size)
    test_one(method, fold)
  }
  
  grid <- expand.grid(m=method_list, s=trial_sizes)
  methods <- grid$m
  sizes <- grid$s
  results <- mcmapply(
    apply_method_to_size,
    methods,
    sizes,
    SIMPLIFY=FALSE,
    mc.cores=detectCores())
  results <- rbind_all(results)
}

stepwise_ols <- function(fold, column_counts=NULL) {
  if (is.null(column_counts)) columns_counts <- 1:ncol(train)
  train <- fold$train
#   # HACK to remove linearly dependant columns
#   model <- lm(FORMULA., data=train)
#   train <- train[, !is.na(coef(model))]
  x <- model.matrix(FORMULA, train)
  y <- train[, TARGET_COLUMN]
  l <- leaps::regsubsets(FORMULA, data=train, method="forward", nvmax=max(column_counts))
  dfs <- lapply(column_counts, function(column_count) {
    columns <- summary(l)$which[column_count, ]
    model <- lm(x=x[, columns], y=y)
    y_hat <- predict(model, test)
    data.frame(
      predicted=y_hat,
      true=test[, TARGET_VARIABLE],
      fold=column_count,
      method='ols')
  })
  rbind_all(dfs)
}

stepwise_tree <- function(fold, column_counts=NULL, threshold=0.4) {
  if (is.null(column_counts)) columns_counts <- 1:ncol(train)
  train <- fold$train
  x <- model.matrix(FORMULA, train)
  y <- train[, TARGET_VARIABLE]
  y <- as.factor(y < quantile(y, threshold))
  v <- varSelRF::varSelRF(x, y)
  
}

plot_scaling <- function(outcome) {
#   g <- group_by(outcome, method, fold)
#   mse <- summarise(g, mse=mean((true-predicted)^2))
#   ggplot(mse, aes(x=fold, y=mse, color=method)) + geom_line() + geom_point()
  reach <- reach_by_pct_targeted(outcome, fold=TRUE, threshold=0.4)
  v <- value_at_pct(reach)
  ggplot(v, aes(x=fold, y=value, color=method)) + geom_line()
}