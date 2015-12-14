library(dplyr)
library(foreign)
library(MLlibrary)

NAME <- 'iraq'

fpath <- paste(TARGETING_DATA_IN, 'iraq_for_prediction_20151118.dta', sep='/')
iraq <- read.dta(fpath)

exclude <- c(
  'znat', # national poverty line, constant
  'zfood', # national poverty constant
  'paasche',
  '_questid',
  'cluster',
  'stratum'
)

df <- select(iraq, -one_of(exclude))
df <- rename_(df, .dots=setNames('pcep', TARGET_VARIABLE))
df <- rename_(df, .dots=setNames('weight', WEIGHT_VARIABLE))
df[, TARGET_VARIABLE] <- log(df[, TARGET_VARIABLE])
df[is.na(df[, WEIGHT_VARIABLE]), WEIGHT_VARIABLE] <- mean(df[, WEIGHT_VARIABLE], na.rm=TRUE)
df$qhada <- as.factor(df$qhada)
for (name in colnames(df)) {
  if (grepl('asset', name)) {
    df[, name] <- as.logical(df[, name])
  }
}

df <- set_aside_holdout(NAME, df)
df <- na_indicator(df)
df <- standardize_predictors(df)
save_dataset(NAME, df)
output <- test_all(df)
save_validation_models_(NAME, output)