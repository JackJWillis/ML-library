context("model")

test_that("kfold_split produces k folds", {
  row_count <- 10
  x <- matrix(1:row_count, nrow=row_count, ncol=1)
  y <- seq_len(row_count)
  seed <- 1
  
  splits <- lapply(1:row_count, function(k) kfold_split(k, y, x, seed)$splits)
  expect_equal(sapply(splits, length), 1:row_count)
})

test_that("x and y are paired after splittng", {
  row_count <- 10
  x <- matrix(1:row_count, nrow=row_count, ncol=1)
  y <- seq_len(row_count)
  seed <- 1
  
  splits <- lapply(1:row_count, function(k) kfold_split(k, y, x, seed))
  for (split in splits) {
    for (fold in split$splits) {
      expect_equal(fold$x_train, fold$y_train)
      expect_equal(fold$x_test, fold$y_test)
    }
  }
})

test_that("splits are even", {
  row_count <- 10
  x <- matrix(1:row_count, nrow=row_count, ncol=1)
  y <- seq_len(row_count)
  seed <- 1
  for (k in 1:row_count) {
    split <- kfold_split(k, y, x, seed)
    for (fold in split$splits) {
      expect_true(abs(length(fold$y_test) - (row_count / k)) <= 1)
    }
  }
})

test_that("trainging and testing are disjoint", {
  row_count <- 10
  x <- matrix(1:row_count, nrow=row_count, ncol=1)
  y <- seq_len(row_count)
  seed <- 1
  for (k in 1:row_count) {
    split <- kfold_split(k, y, x, seed)$splits
    for (fold in split) {
      train <- fold$x_train
      test <- fold$x_test
      expect_true(length(intersect(train, test)) == 0)
      
      train <- fold$y_train
      test <- fold$y_test
      expect_true(length(intersect(train, test)) == 0)
    }
  }
})

test_that("trainging and testing contain all the data", {
  row_count <- 10
  x <- matrix(1:row_count, nrow=row_count, ncol=1)
  y <- seq_len(row_count)
  seed <- 1
  for (k in 1:row_count) {
    split <- kfold_split(k, y, x, seed)$splits
    for (fold in split) {
      train <- fold$x_train
      test <- fold$x_test
      expect_true(setequal(union(train, test), x))
      
      train <- fold$y_train
      test <- fold$y_test
      expect_true(setequal(union(train, test), y))
    }
  }
})


test_that("can reproduce islr", {
  # From Introduction to Statistical Learning, Chapter 6
  Hitters <- ISLR::Hitters
  Hitters <- na.omit(Hitters)
  x <- model.matrix(Salary ~ ., Hitters)[, -1]
  y <- Hitters$Salary
  seed <- 100
  set.seed(seed)
  split <- kfold_split(2, y, x, seed)$splits[[1]]
  y_train <- split$y_train
  y_test <- split$y_test
  x_test <- split$x_test
  
  ls <- do.call(LeastSquares(), split)
  ls_fit <- fit(ls)
  ls_mse <- mean((predict(ls, ls_fit) - y_test) ^ 2)

  ridge <- do.call(Ridge(), split)
  ridge_fit <- fit(ridge)
  ridge_mse <- mean((predict(ridge, ridge_fit) - y_test) ^ 2)

  lasso <- do.call(Lasso(), split)
  lasso_fit <- fit(lasso)
  lasso_mse <- mean((predict(lasso, lasso_fit)- y_test) ^ 2)
  
  mean_mse <- mean((mean(y_train) - y_test) ^ 2)

  print(ls_mse)
  print(ridge_mse)
  print(lasso_mse)
  print(mean_mse)

})
