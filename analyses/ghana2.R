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

NAME <- "ghana"

# Load data ---------------------------

load_data <- function(data_path) {
  read.dta(data_path)
}

add_covariates <- function(output_df, ghana, var_table_path) {
  feature_info <- read.xlsx(var_table_path, sheetName="Sheet1")
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
  output_df[, TARGET_VARIABLE] <- log(panel_df$lnwelfare)
  output_df
}

remove_missing_data <- function(output_df) {
  output_df[complete.cases(output_df), ]
}

create_dataset <- function(data_path, var_table_path, remove_missing=TRUE) {
  ghana <- load_data(data_path)
  df <-
    matrix(nrow=nrow(ghana), ncol=0) %>%
    data.frame() %>%
    add_covariates(ghana, var_table_path) %>%
    add_target(ghana) 
  if (remove_missing) df <- remove_missing_data(df)
  df
}

# Run analysis ---------------------------
PE_DATA_FNAME <- "pre_feature_extraction.dta"
PE_VARIABLE_TABLE_FNAME <- "variable_table_ghana_pre_extraction.xlsx"

DATA_FNAME <- "model_a_vars1.dta"
VARIABLE_TABLE_FNAME <- "variable_table_ghana.xlsx"

data_path <- paste(TARGETING_DATA_IN, DATA_FNAME, sep="/")
variable_table_path <- paste(TARGETING_DATA_IN, VARIABLE_TABLE_FNAME, sep="/")

pe_data_path <- paste(TARGETING_DATA_IN, PE_DATA_FNAME, sep="/")
pe_variable_table_path <- paste(TARGETING_DATA_IN, PE_VARIABLE_TABLE_FNAME, sep="/")

gh <- create_dataset(data_path, variable_table_path)
gh <- filter(gh, s7dq11 != 'generator') # only one observation, causes issues in cross validation
gh <- standardize_predictors(gh, TARGET_VARIABLE)
save_dataset(NAME, gh)
output <- test_all(gh)
save_validation_models_(NAME, output)



gh <- create_dataset(pe_data_path, pe_variable_table_path)
gh <- filter(gh, s7dq11 != 'generator') # only one observation, causes issues in cross validation
gh <- standardize_predictors(gh, TARGET_VARIABLE)
save_dataset(paste(NAME, 'pe', sep='_'), gh)
output <- test_all(gh)
save_validation_models_(paste(NAME, 'pe', sep='_'), output)
