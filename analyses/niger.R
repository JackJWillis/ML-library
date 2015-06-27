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

load_data <- function() {
  read.dta(DATA_PATH)
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
  output_df[, c(covariates_yesno)] <- lapply(output_df[, c(covariates_yesno)], as.logical)

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

# Import data ---------------------------

TARGETING_DATA_IN <- "C:/Users/Jack/Box Sync/Poverty Targeting (DATA)/LSMS_ISA/Niger/Data/tmp"
DATA_FNAME <- "xxx_no_accents.dta"
DATA_PATH <- paste(TARGETING_DATA_IN, DATA_FNAME, sep="/")

VARIABLE_TABLE_FNAME <- "variable_table_niger.xlsx"
VARIABLE_TABLE_PATH <- paste("C:/Users/Jack/Box Sync/Poverty Targeting (DATA)/Niger", VARIABLE_TABLE_FNAME, sep="/")


# Just using their variables. Note, different for different agricultural zones

NAME <- "niger_pastoral"

temp <- load_data()

#Just keeping Pastorale:
pastoral <- temp[temp$milieu == "Pastorale",]
niger_p <- create_dataset(pastoral)
niger_p_weight <- niger_p$hhweight
niger_p$hhweight <- NULL
niger_p_id <- data.frame(grappe=niger_p$grappe,menage=niger_p$menage)
niger_p$grappe <- NULL
niger_p$menage <- NULL
niger_p <- standardize_predictors(niger_p, "y_real")
save_dataset(NAME, niger_p)

x_p <- model.matrix(y_real ~ .,  niger_p)
x_p_nmm <- select(niger_p,-one_of("y_real"))
y_p <- niger_p[rownames(x_p), "y_real"]

NAME <- "niger_agricultural"

temp <- load_data()
#Just keeping Pastorale:
agricultural <- temp[temp$milieu %in% c("Agricole","Agropastorale"),]
niger_a <- create_dataset(agricultural)
niger_a_weight <- niger_a$hhweight
niger_a$hhweight <- NULL
niger_a_id <- data.frame(grappe=niger_a$grappe,menage=niger_a$menage)
niger_a$grappe <- NULL
niger_a$menage <- NULL
niger_a <- standardize_predictors(niger_a, "y_real")
save_dataset(NAME, niger_a)

x_a <- model.matrix(y_real ~ .,  niger_a)
x_a_nmm <- select(niger_a,-one_of("y_real"))
y_a <- niger_a[rownames(x_a), "y_real"]

# Set k ----------------------------------------

k <- 5

# Function to run all models -------------------

run_models = function(y,x,x_nmm,NAME) {
  
  print("Running ridge")
  ridge <- kfold(k, Ridge(), y, x)
  print("Running lasso")
  lasso <- kfold(k, Lasso(), y, x)
  print("Running least squares")
  least_squares <- kfold(k, LeastSquares(), y, x)
  
#   x_ix <- model.matrix(~ . + .:.,  x_nmm)
#   y_ix <- y
#   print("Running ridge with interactions")
#   ridge_ix <- kfold(k, Ridge(), y_ix, x_ix)
#   print("Running lasso with interactions")
#   lasso_ix <- kfold(k, Lasso(), y_ix, x_ix)
#   print("Running least squares with interactions")
#   least_squares_ix <- kfold(k, LeastSquares(), y_ix, x_ix)
  
#   print("Running rtree")
#   rtree <- kfold(k, rTree2(), y, x_nmm)  
#   print("Running randomForest")
#   forest <- kfold(k, Forest(), y, x_nmm)
# #   print("Running mca")
# #   mca_knn <- kfold(k, MCA_KNN(ndim=12, k=5), y, x_nmm)
# 
#   threshold_20 <- quantile(y, .2)
#   print("Running logistic")
#   logistic_20 <- kfold(k, Logistic(threshold_20), y, x)
#   print("Running logisitic lasso")
#   logistic_lasso_20 <- kfold(k, LogisticLasso(threshold_20), y, x)
#   print(" Running ctree")
#   # ctree_20 <- kfold(k, cTree2(threshold_20), y, x_nmm)
#   print("Running randomForest")
#   cforest_20 <- kfold(k, cForest(threshold_20), y, x_nmm)
#   
#   threshold_30 <- quantile(y, .3)
#   print("Running logistic")
#   logistic_30 <- kfold(k, Logistic(threshold_30), y, x)
#   print("Running logisitic lasso")
#   logistic_lasso_30 <- kfold(k, LogisticLasso(threshold_30), y, x)
#   print(" Running ctree")
#   # ctree_30 <- kfold(k, cTree2(threshold_30), y, x_nmm)
#   print("Running randomForest")
#   cforest_30 <- kfold(k, cForest(threshold_30), y, x_nmm)
  
#   save_models(NAME,
#               least_squares=least_squares,
#               ridge=ridge,
#               lasso=lasso,
#               rtree=rtree,
#               forest=forest,
#               ridge_ix=ridge_ix,
#               lasso_ix=lasso_ix,
#               least_squares_ix=least_squares_ix)
  
  output_wide <- data.frame(y_real = y)
  models <- list(least_squares=least_squares,
                 ridge=ridge,
                 lasso=lasso)
  for( name in names(models)) {
    output_wide <- data.frame(output_wide,models[[name]]$predicted)
    colnames(output_wide)[ncol(output_wide)] <- name
  }
  output_wide
} 

# Generate output ---------------------------------

TARGETING_DATA_OUT <- "data"

niger_p_models <- run_models(y_p,x_p,x_p_nmm,"niger_pastoral")
niger_p_models$hhweight <- niger_p_weight
niger_p_models <- data.frame(niger_p_models,niger_p_id)
out_path <- paste(TARGETING_DATA_OUT, "niger_p_wide.csv", sep="/")
write.csv(niger_p_models, file=out_path)

niger_a_models <- run_models(y_a,x_a,x_a_nmm,"niger_agricultural")
niger_a_models$hhweight <- niger_a_weight
niger_a_models <- data.frame(niger_a_models,niger_a_id)
out_path <- paste(TARGETING_DATA_OUT, "niger_a_wide.csv", sep="/")
write.csv(niger_a_models, file=out_path)


