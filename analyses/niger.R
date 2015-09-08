#####################################################################
## This file takes file does analysis for Niger and outputs a stata file with the different predictions
#####################################################################

library(magrittr)
library(foreign)
library(xlsx)
library(dplyr)
library(MLlibrary)


# TODO: 
# Add weights.... 
# Ask Jonathan if any more functions


# Load data functions ---------------------------

load_data <- function(path) {
  read.dta(path)
}

add_covariates <- function(output_df, input_df) {
  feature_info <- read.xlsx(VARIABLE_TABLE_PATH, sheetName="Sheet1")
  # Temp fix
#   feature_info$var_name <- as.character(feature_info$var_name)
#   feature_info$var_name[feature_info$var_name=="as4aq11BÅ“uf"] <- "as4aq11Bœuf"
#   feature_info$var_name[feature_info$var_name=="as4aq05ChÃ¨vre"] <- "as4aq05Chèvre"    
#   feature_info$var_name[feature_info$var_name=="as4aq11ChÃ¨vre"] <- "as4aq11Chèvre"
#   feature_info$var_name[feature_info$var_name=="as4aq05Mouton_Bilier"] <- "as4aq05Mouton_Bélier"
  
  feature_info <- feature_info[!is.na(feature_info$var_name), ]
  select_names_by_type <- function(type) {
    is_desired_type <- feature_info$type == type
    as.vector(feature_info$var_name[is_desired_type])
  }
  columns <- colnames(input_df)
  
  covariates_categorical <- select_names_by_type("F")
  covariates_cardinal <- select_names_by_type("C")
  covariates_yesno <- select_names_by_type("Y")
  covariates <- c(covariates_categorical, covariates_cardinal, covariates_yesno)
  
  # Add features to output
  output_df[, c(covariates_categorical)] <- input_df[, c(covariates_categorical)]
  output_df[, c(covariates_cardinal)] <- input_df[, c(covariates_cardinal)]
  output_df[, c(covariates_yesno)] <- input_df[, c(covariates_yesno)]
  
  # Make sure features are cast correctly
  output_df[, c(covariates_categorical)] <- lapply(output_df[, c(covariates_categorical)], as.factor)
  output_df[, c(covariates_cardinal)] <- lapply(output_df[, c(covariates_cardinal)], as.numeric)
  # Note, may wish to recode some of the categoricals as ordered and the yes/no as categorical.
  output_df[, c(covariates_yesno)] <- lapply(output_df[, c(covariates_yesno)], as.factor)

  #Add weights
  output_df[, c("hhweight")] <- input_df[, c("hhweight")]
  #Add ID
  output_df[, c("grappe","menage")] <- input_df[, c("grappe","menage")]
  output_df
}


add_target <- function(output_df, panel_df) {
  output_df$y_real <- panel_df$Y 
  output_df
}

remove_missing_data <- function(output_df) {
  output_df[complete.cases(output_df), ]
}

create_dataset <- function(input_df,remove_missing=TRUE) {
  df <-
    matrix(nrow=nrow(input_df), ncol=0) %>%
    data.frame() %>%
    add_covariates(input_df) %>%
    add_target(input_df) 
  if (remove_missing) df <- remove_missing_data(df)
  df
}

create_dataset_pmt <- function(input_df,remove_missing=TRUE) {
  df <-
    matrix(nrow=nrow(input_df), ncol=0) %>%
    data.frame() %>%
    add_covariates(input_df, pmt_only=TRUE) %>%
    add_target(input_df) 
  if (remove_missing) df <- remove_missing_data(df)
  df
}

# Import data ---------------------------

# TARGETING_DATA_IN <- "C:/Users/Jack/Box Sync/Poverty Targeting (DATA)/LSMS_ISA/Niger/Data/tmp"
# DATA_FNAME <- "xxx_no_accents.dta"
# DATA_PATH <- paste(TARGETING_DATA_IN, DATA_FNAME, sep="/")
# 
# VARIABLE_TABLE_FNAME <- "variable_table_niger.xlsx"
# VARIABLE_TABLE_PATH <- paste("C:/Users/Jack/Box Sync/Poverty Targeting (DATA)/Niger", VARIABLE_TABLE_FNAME, sep="/")

DATA_FNAME <- "xxx_no_accents.dta"
VARIABLE_TABLE_FNAME <- "variable_table_niger.xlsx"

DATA_PATH <- paste(TARGETING_DATA_IN, DATA_FNAME, sep="/")
VARIABLE_TABLE_PATH <- paste(TARGETING_DATA_IN, VARIABLE_TABLE_FNAME, sep="/")

DATA_PATH_PMT <- paste(TARGETING_DATA_IN, 'FINAL_PMT.dta', sep="/")
# Just using their variables. Note, different for different agricultural zones

temp <- load_data(DATA_PATH)

#Just keeping Pastorale:
pastoral <- temp[temp$milieu == "Pastorale",]
niger_p <- create_dataset(pastoral)
niger_p_weight <- niger_p$hhweight
niger_p$hhweight <- NULL
niger_p_id <- data.frame(grappe=niger_p$grappe,menage=niger_p$menage)
niger_p$grappe <- NULL
niger_p$menage <- NULL
niger_p <- standardize_predictors(niger_p, "y_real")

x_p <- model.matrix(y_real ~ .,  niger_p)
y_p <- niger_p[rownames(x_p), "y_real"]
x_p_nmm <- select(niger_a, -one_of('y_real'))

temp <- load_data(DATA_PATH)
#Just keeping Pastorale:
agricultural <- temp[temp$milieu %in% c("Agricole","Agropastorale"),]
niger_a <- create_dataset(agricultural)
niger_a_weight <- niger_a$hhweight
niger_a$hhweight <- NULL
niger_a_id <- data.frame(grappe=niger_a$grappe,menage=niger_a$menage)
niger_a$grappe <- NULL
niger_a$menage <- NULL
niger_a <- standardize_predictors(niger_a, "y_real")

x_a <- model.matrix(y_real ~ .,  niger_a)
x_a_nmm <- select(niger_a, -one_of('y_real'))
y_a <- niger_a[rownames(x_a), "y_real"]


cv_splits_a <- cv_split(y_a, x_a_nmm, k=5, inner_k=4, seed=1, weight=niger_a_weight)
run_all_heldout('niger_agricultural', niger_a, 'y_real', cv_splits_a)
run_fs_heldout('niger_agricultural_25', niger_a, 'y_real', cv_splits_a)

cv_splits_p <- cv_split(y_p, x_p_nmm, k=5, inner_k=4, seed=1, weight=niger_p_weight)
run_all_heldout('niger_pastoral', niger_p, 'y_real', cv_splits_p)
run_fs_heldout('niger_pastoral_25', niger_p, 'y_real', cv_splits_p)


feature_info <- read.xlsx(VARIABLE_TABLE_PATH, sheetName="Sheet1")
feature_info[, "Agro.and.Agro.Pastoral.zone"] <- as.numeric(as.character(feature_info[, "Agro.and.Agro.Pastoral.zone"]))
feature_info[, "Pastoral.zone"] <- as.numeric(as.character(feature_info[, "Pastoral.zone"]))
feature_info[, "var_name"] <- as.character(feature_info[, "var_name"])
agricultural_pmt <- select(niger_a, -one_of(feature_info$var_name[!is.na(feature_info$Agro.and.Agro.Pastoral.zone)]))
pastoral_pmt <- select(niger_p, -one_of(feature_info$var_name[!is.na(feature_info$Pastoral.zone)]))

# x_a <- model.matrix(y_real ~ .,  agricultural_pmt)
# y_a <- niger_a[rownames(x_a), "y_real"]
# cv_splits_a <- cv_split(y_a, x_a, k=5, inner_k=3, seed=1, weight=niger_a_weight)
# run_all_heldout('niger_agricultural_pmt', niger_a, 'y_real', cv_splits_a)
# 
# x_p <- model.matrix(y_real ~ .,  pastoral_pmt)
# y_p <- niger_p[rownames(x_p), "y_real"]
# cv_splits_p <- cv_split(y_p, x_p, k=5, inner_k=3, seed=1, weight=niger_p_weight)
# run_all_heldout('niger_pastoral_pmt', niger_p, 'y_real', cv_splits_p)