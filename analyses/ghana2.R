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
  if (remove_missing) df <- remove_missing_data(df)
  df
}

# Run analysis ---------------------------

gh_missing <- create_dataset(remove_missing=FALSE)
gh <- create_dataset()
gh <- standardize_predictors(gh, "lnwelfare")
save_dataset(NAME, gh)
x <- model.matrix(lnwelfare ~ .,  gh)
x_nmm <- select(gh,-one_of("lnwelfare"))
y <- gh[rownames(x), "lnwelfare"]
k <- 5

print("Running ridge")
ridge <- kfold(k, Ridge(), y, x)
print("Running lasso")
lasso <- kfold(k, Lasso(), y, x)
print("Running least squares")
least_squares <- kfold(k, LeastSquares(), y, x)

print("Running stepwise")
stepwise <- kfold(k, Stepwise(300), y, x)

print("Running grouped ridge")
x_nmm$rural <- as.factor(x_nmm$rural)
ridge_rural <- kfold(k, GroupedRidge("rural"), y, x_nmm)
#ERROR: Not sure why.
#Do this later for zone which Pascale mentioned


print("Running rtree")
rtree <- kfold(k, rTree2(), y, x_nmm)

# print("Running randomForest")
# forest <- kfold(k, Forest(), y, x_nmm)

print("Running mca")
mca_knn <- kfold(k, MCA_KNN(ndim=12, k=5), y, x_nmm)
print("Running pca")
pca_knn <- kfold(k, PCA_KNN(ndim=12, k=5), y, x_nmm)

mca_pca_avg <- mca_knn
mca_pca_avg$predicted <- (mca_pca_avg$predicted + pca_knn$predicted) / 2


threshold_20 <- quantile(gh$lnwelfare, .2)
print("Running logistic")
logistic_20 <- kfold(k, Logistic(threshold_20), y, x)
print("Running logisitic lasso")
logistic_lasso_20 <- kfold(k, LogisticLasso(threshold_20), y, x)
print(" Running ctree")
ctree_20 <- kfold(k, cTree2(threshold_20), y, x_nmm)
print("Running randomForest")
cforest_20 <- kfold(k, cForest(threshold_20), y, x_nmm)

threshold_30 <- quantile(gh$lnwelfare, .3)
print("Running logistic")
logistic_30 <- kfold(k, Logistic(threshold_30), y, x)
print("Running logisitic lasso")
logistic_lasso_30 <- kfold(k, LogisticLasso(threshold_30), y, x)
print(" Running ctree")
ctree_30 <- kfold(k, cTree2(threshold_30), y, x_nmm)
print("Running randomForest")
cforest_30 <- kfold(k, cForest(threshold_30), y, x_nmm)

save_models(NAME,
            ridge=ridge,
            lasso=lasso,
            least_squares=least_squares,
            stepwise=stepwise,
            #ridge_muni=ridge_muni,
            #ridge_state=ridge_state,
            # rtree=rtree,
            #mca_knn=mca_knn,
            #pca_knn=pca_knn,
            #mca_pca_avg=mca_pca_avg,
            logistic_20=logistic_20,
            logistic_lasso_20=logistic_lasso_20,
            ctree_20=ctree_20,
            cforest_20=cforest_20,
            logistic_lasso_30=logistic_lasso_30,
            ctree_30=ctree_30,
            cforest_30=cforest_30)