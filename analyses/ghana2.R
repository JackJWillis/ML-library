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

NAME <- "ghana_tuned"

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
  output_df[, covariates_categorical] <- ghana[, covariates_categorical]
  output_df[, covariates_cardinal] <- ghana[, covariates_cardinal]
  output_df[, covariates_yesno] <- ghana[, covariates_yesno]
  
  # Make sure features are cast correctly
  output_df[, covariates_categorical] <- lapply(output_df[, covariates_categorical], as.factor)
  output_df[, covariates_cardinal] <- lapply(output_df[, covariates_cardinal], as.numeric)
  # Note, may wish to recode some of the categoricals as ordered and the yes/no as categorical.
  output_df[, covariates_yesno] <- lapply(output_df[, covariates_yesno], as.factor)
  
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
    as.data.frame() %>%
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

gh <- create_dataset(data_path, variable_table_path, remove_missing=FALSE)
gh <- standardize_predictors(gh, TARGET_VARIABLE)
gh <- na_indicator(gh)
save_dataset(NAME, gh)
clear_config(NAME)
output <- test_all_named(NAME, gh, test_fraction=0.2)
save_validation_models_(NAME, output)



gh <- create_dataset(pe_data_path, pe_variable_table_path, remove_missing=FALSE)
gh <- filter(gh, s7dq11 != 'generator') # only one observation, causes issues in cross validation
gh <- na_indicator(gh)
gh <- standardize_predictors(gh, TARGET_VARIABLE)
pe_name <- paste(NAME, 'pe', sep='_')
save_dataset(pe_name, gh)
clear_config(pe_name)
output <- test_all_named(pe_name, gh, test_fraction=0.2)
save_validation_models_(pe_name, output)
