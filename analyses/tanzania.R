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



DATA_PATH = "inst/extdata/intermediate_tanzania_for_R_1.dta"
VARIABLE_TABLE_PATH = "inst/extdata/variable_table_tanzania.xlsx"

# setwd("C:/Users/Jack/Box Sync/Poverty Targeting/LSMS_ISA/Tanzania/")
# data_in = "intermediate/data/intermediate_tanzania_for_R_1.dta"
# var_table_tanzania = "variable_tables/variable_table_tanzania.xlsx"


# Load data ---------------------------

load_data <- function() {
  read.dta(DATA_PATH)
}

add_covariates <- function(output_df, tanzania_panel) {
  feature_info <- read.xlsx(VARIABLE_TABLE_PATH, sheetName="Var (Final)")

  # Select features and determine feature types
  # Just using priority one variables for now
  is_priority_one <- feature_info$priority == 1
  exists_2008_09 <- feature_info$exists_2008_09 == 1
  select_names_by_type <- function(type) {
    is_desired_type <- feature_info$type == type
    as.vector(feature_info$var_name[is_priority_one & exists_2008_09 & is_desired_type])
  }
  
  covariates_categorical <- select_names_by_type("Categorical")
  covariates_cardinal <- select_names_by_type("Cardinal")
  covariates_yesno <- select_names_by_type("Yes/No")
  covariates <- c(covariates_categorical, covariates_cardinal, covariates_yesno)

  # Add features to output
  output_df[, c(covariates_categorical)] <- tanzania_panel[, c(covariates_categorical)]
  output_df[, c(covariates_cardinal)] <- tanzania_panel[, c(covariates_cardinal)]
  output_df[, c(covariates_yesno)] <- tanzania_panel[, c(covariates_yesno)]
  
  # Make sure features are cast correctly
  output_df[, c(covariates_categorical)] <- lapply(output_df[, c(covariates_categorical)], as.factor)
  output_df[, c(covariates_cardinal)] <- lapply(output_df[, c(covariates_cardinal)], as.numeric)
  # Note, may wish to recode some of the categoricals as ordered and the yes/no as categorical.
  output_df[, c(covariates_yesno)] <- lapply(output_df[, c(covariates_yesno)], as.logical)

  output_df
}

add_target_per_capita <- function(output_df, panel_df) {
  output_df$lconsPC <- log(panel_df$expmR / panel_df$hhsize) 
  output_df
}

add_target_adult_equivalent <- function(output_df, panel_df) {
  output_df$lconsPAE <- log(panel_df$expmR %/% panel_df$adulteq)
  output_df
}

remove_missing_data <- function(output_df) {
  output_df[complete.cases(output_df), ]
}

select_year <- function(output_df, panel_df, year) {
  output_df[panel_df$year == year, ]
}

create_dataset <- function(year, remove_missing=TRUE) {
  tanzania_panel <- load_data()
  df <-
    matrix(nrow=nrow(tanzania_panel), ncol=0) %>%
    data.frame() %>%
    add_covariates(tanzania_panel) %>%
    add_target_per_capita(tanzania_panel) %>%
    select_year(tanzania_panel, year) 
  if (remove_missing) remove_missing_data(df)
  df
}

# Run analysis ---------------------------

tz08 <- create_dataset(2008)
x <- model.matrix(lconsPC ~ ., tz08)
y <- tz08[rownames(x), "lconsPC"]
k <- 10

ridge <- kfold(k, ridge_predict, y, x)
lasso <- kfold(k, lasso_predict, y, x)
least_squares <- kfold(k, least_squares_predict, y, x)