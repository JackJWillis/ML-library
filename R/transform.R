#' @export

standardize_predictors  <-  function(df, target=TARGET_VARIABLE, weight=WEIGHT_VARIABLE) {
  constant_or_empty <- function(values) {
    all(is.na(values)) || all(values[1] == values)
  }
  standard <- dplyr::select(df, -one_of(target, weight))

  numeric_features <- sapply(standard, is.numeric)
  standard[, numeric_features] <- scale(standard[, numeric_features], center=TRUE, scale=TRUE)

  degenerate <- sapply(standard, constant_or_empty)
  standard <- standard[, !degenerate]

  standard[, target] <- df[, target]
  if (weight %in% colnames(df)) {
    standard[, weight] <- df[, weight]
  }
  standard
}

MISSINGNESS_INDICATOR <- "missing_missing_missing"
na_indicator <- function(df) {
  for (name in names(df)) {
    if (any(is.na(df[[name]]))) {
      if (is.factor(df[[name]])) {
        levels(df[[name]]) <- c(levels(df[[name]]), MISSINGNESS_INDICATOR)
        df[is.na(df[[name]]), name] <- MISSINGNESS_INDICATOR
      }
      if (is.numeric(df[[name]])) {
        df[[paste(name, "NA", sep=".")]] <- is.na(df[[name]])
        df[is.na(df[[name]]), name] <- 0
      }
    }
  }
  df
}

impute_all <- function(df) {
  for (name in colnames(df)) {
    vals <- df[, name]
    if (is.numeric(vals)) {
      df[is.na(vals), name] <- median(vals)
    }
    if (is.character(vals) | is.factor(vals)) {
      df[is.na(vals), name] <- names(sort(table(vals), decreasing=TRUE))[1]
    }
  }
  df
}

knockout_new_categories <- function(test, train) {
  for (name in intersect(colnames(test), colnames(train))) {
    vals <- test[, name]
    if (is.character(vals) | is.factor(vals)) {
      test[!(vals %in% unique(train[, name])), name] <- NA
    }
  }
  test
}

