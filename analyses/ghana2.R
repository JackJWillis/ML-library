#####################################################################
## This file takes file intermediate_tanzania_for_R_1 and uses it to
## compare linear regression, LASSO, Ridge regressions and regresion trees.
## It compares the techniques for predicting consumption and poverty,
## using baseline covariates, both for the baseline year and future years
#####################################################################

library(magrittr)
library(foreign)
library(xlsx)
library(dplyr)
library(MLlibrary)


DATA_FNAME <- "model_a_vars1.dta"
VARIABLE_TABLE_FNAME <- "variable_table_ghana.xlsx"

DATA_PATH <- paste(TARGETING_DATA_IN, DATA_FNAME, sep="/")
VARIABLE_TABLE_PATH <- paste(TARGETING_DATA_IN, VARIABLE_TABLE_FNAME, sep="/")
NAME <- "ghana"

# Load data ---------------------------

load_data <- function() {
  read.dta(DATA_PATH)
}

add_covariates <- function(output_df, ghana) {
  feature_info <- read.xlsx(VARIABLE_TABLE_PATH, sheetName="Sheet1")
  feature_info <- feature_info[!is.na(feature_info$var_name), ]
  select_names_by_type <- function(type) {
    is_desired_type <- feature_info$type == type
    as.vector(feature_info$var_name[is_desired_type])
  }
  
  covariates_categorical <- select_names_by_type("Categorical")
  covariates_cardinal <- select_names_by_type("Cardinal")
  covariates_yesno <- select_names_by_type("Yes/No")
  covariates <- c(covariates_categorical, covariates_cardinal, covariates_yesno)
  
  # Add features to output
  output_df[, c(covariates_categorical)] <- ghana[, c(covariates_categorical)]
  output_df[, c(covariates_cardinal)] <- ghana[, c(covariates_cardinal)]
  output_df[, c(covariates_yesno)] <- ghana[, c(covariates_yesno)]
  
  # Make sure features are cast correctly
  output_df[, c(covariates_categorical)] <- lapply(output_df[, c(covariates_categorical)], as.factor)
  output_df[, c(covariates_cardinal)] <- lapply(output_df[, c(covariates_cardinal)], as.numeric)
  # Note, may wish to recode some of the categoricals as ordered and the yes/no as categorical.
  output_df[, c(covariates_yesno)] <- lapply(output_df[, c(covariates_yesno)], as.factor)
  
  output_df
}

add_target <- function(output_df, panel_df) {
  output_df$lnwelfare <- log(panel_df$lnwelfare) 
  output_df
}

remove_missing_data <- function(output_df) {
  output_df[complete.cases(output_df), ]
}

create_dataset <- function(remove_missing=TRUE) {
  ghana <- load_data()
  df <-
    matrix(nrow=nrow(ghana), ncol=0) %>%
    data.frame() %>%
    add_covariates(ghana) %>%
    add_target(ghana) 
  if (remove_missing) df <- remove_missing_data(df)
  df
}

# Run analysis ---------------------------

gh <- create_dataset()
gh <- standardize_predictors(gh, "lnwelfare")
save_dataset(NAME, gh)
x <- model.matrix(lnwelfare ~ .,  gh)
x_nmm <- select(gh,-one_of("lnwelfare"))
y <- gh[rownames(x), "lnwelfare"]
k <- 5

ksplit <- kfold_split(k, y, x, seed=1)
ksplit_nmm <- kfold_split(k, y, x_nmm, seed=1)
run_all_models(NAME, gh, "lnwelfare", ksplit, ksplit_nmm, 'rural')