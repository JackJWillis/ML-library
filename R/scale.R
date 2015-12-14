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

stepwise_ols <- function(fold, column_counts=NULL, holdout_fraction=0.2) {
  if (is.null(column_counts)) column_counts <- 1:ncol(survey_df)
  
  is_test <- as.logical(rbinom(nrow(survey_df), 1, holdout_fraction))
  x <- model.matrix(FORMULA, survey_df)
  df <- data.frame(x)
  y <- survey_df[rownames(x), TARGET_VARIABLE]
  df[, TARGET_VARIABLE] <- y
  train <- data.frame(df[!is_test, ])
  train_mm <- x[!is_test, ]
  test <- data.frame(df[is_test, ])
  
  l <- leaps::regsubsets(x=train_mm, y=y, intercept=FALSE, method="forward", nvmax=max(column_counts))
  dfs <- lapply(column_counts, function(column_count) {
    columns <- summary(l)$which[column_count, ]
    columns[1] <- FALSE
    columns <- names(columns)[columns]
    model <- lm(FORMULA, data=train[, c(columns, TARGET_VARIABLE)])
    y_hat <- predict(model, test)
    data.frame(
      predicted=y_hat,
      true=test[, TARGET_VARIABLE],
      fold=column_count,
      method='ols')
  })
  rbind_all(dfs) %>%
    group_by(fold) %>%
    summarise(method=first(method), mse=mean((predicted - true)^2))
}

stepwise_tree <- function(survey_df, threshold=0.4) {
  x <- model.matrix(FORMULA, survey_df)
  y <- survey_df[, TARGET_VARIABLE]
  y <- as.factor(y < quantile(y, threshold))
  v <- varSelRF::varSelRF(x, y)
  var_history <- as.character(v$selec.history$Vars.in.Forest)
  var_counts <- sapply(strsplit(var_history, '[+]'), length)
  mse <- v$selec.history$OOB
  data.frame(method='forest', mse=mse, fold=var_counts)
}

scale_k <- function(survey_df) {
  ols_results <- stepwise_ols(survey_df)
  tree_results <- stepwise_tree(survey_df)
  rbind(ols_results, tree_results)
}


plot_scaling <- function(outcome) {
#   g <- group_by(outcome, method, fold)
#   mse <- summarise(g, mse=mean((true-predicted)^2))
#   ggplot(mse, aes(x=fold, y=mse, color=method)) + geom_line() + geom_point()
  reach <- reach_by_pct_targeted(outcome, fold=TRUE, threshold=0.4)
  v <- value_at_pct(reach)
  ggplot(v, aes(x=fold, y=value, color=method)) + geom_line()
}