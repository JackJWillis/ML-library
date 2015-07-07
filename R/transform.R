#' @export

standardize_predictors  <-  function(df, target) {
  constant_or_empty <- function(values) {
    all(is.na(values)) || all(values[1] == values)
  }
  standard <- dplyr::select(df, -matches(target))

  numeric_features <- sapply(standard, is.numeric)
  standard[, numeric_features] <- scale(standard[, numeric_features], center=TRUE, scale=TRUE)

  degenerate <- sapply(standard, constant_or_empty)
  standard <- standard[, !degenerate]

  standard[, target] <- df[, target]
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


