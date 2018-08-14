#####################################################################
## This file takes file intermediate_tanzania_for_R_1 and uses it to
## compare linear regression, LASSO, Ridge regressions and regresion trees.
## It compares the techniques for predicting consumption and poverty,
## using baseline covariates, both for the baseline year and future years
#####################################################################

library(magrittr)
library(foreign)
library(xlsx)

library(MLlibrary)

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
  output_df[, c(covariates_yesno)] <- lapply(output_df[, c(covariates_yesno)], as.logical)
  
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
  if (remove_missing) remove_missing_data(df)
  df
}

# Create first dataset --------------------------- 

DATA_FNAME <- "model_a_vars1.dta"
VARIABLE_TABLE_FNAME <- "variable_table_ghana.xlsx"

DATA_PATH <- paste(TARGETING_DATA_IN, DATA_FNAME, sep="/")
VARIABLE_TABLE_PATH <- paste(TARGETING_DATA_IN, VARIABLE_TABLE_FNAME, sep="/")
NAME <- "ghana_pe"

gh_missing <- create_dataset(remove_missing=FALSE)
gh <- create_dataset()
gh <- standardize_predictors(gh, "lnwelfare")
save_dataset(NAME, gh)
x <- model.matrix(lnwelfare ~ .,  gh)
y <- gh[rownames(x), "lnwelfare"]

x_ix <- model.matrix(lnwelfare ~ . + .:.,  gh)
y_ix <- gh[rownames(x_ix), "lnwelfare"]

# Create second dataset --------------------------- 

DATA_FNAME <- "pre_feature_extraction.dta"
VARIABLE_TABLE_FNAME <- "variable_table_ghana_pre_extraction.xlsx"

DATA_PATH <- paste(TARGETING_DATA_IN, DATA_FNAME, sep="/")
VARIABLE_TABLE_PATH <- paste(TARGETING_DATA_IN, VARIABLE_TABLE_FNAME, sep="/")
NAME <- "ghana_pe"

gh_pe_missing <- create_dataset(remove_missing=FALSE)
gh_pe <- create_dataset()
gh_pe <- standardize_predictors(gh_pe, "lnwelfare")
save_dataset(NAME, gh_pe)
x_pe <- model.matrix(lnwelfare ~ .,  gh_pe)
y_pe <- gh_pe[rownames(x), "lnwelfare"]

x_pe_ix <- model.matrix(lnwelfare ~ . + .:.,  gh_pe)
y_pe_ix <- gh[rownames(x_pe_ix), "lnwelfare"]

# Run analysis ---------------------------

k <- 5
print("Running lasso")
lasso <- kfold(k, Lasso(), y, x)
print("Running least squares")
least_squares <- kfold(k, LeastSquares(), y, x)
print("Running lasso PE")
lasso_pe <- kfold(k, Lasso(), y_pe, x_pe)
print("Running least squares PE")
least_squares_pe <- kfold(k, LeastSquares(), y_pe, x_pe)
print("Running lasso IX")
lasso_ix <- kfold(k, Lasso(), y_ix, x_ix)
print("Running least squares IX")
least_squares_ix <- kfold(k, LeastSquares(), y_ix, x_ix)
# print("Running lasso PE IX")
# lasso_pe_ix <- kfold(k, Lasso(), y_pe_ix, x_pe_ix)
# print("Running least squares PE IX")
# least_squares_pe_ix <- kfold(k, LeastSquares(), y_pe_ix, x_pe_ix)

#Add pe_ix in following if will run
save_models(NAME,
            lasso=lasso,
            least_squares=least_squares,
            lasso_pe=lasso_pe,
            least_squares_pe=least_squares_pe,            
            lasso_ix=lasso_ix,
            least_squares_ix=least_squares_ix
            )