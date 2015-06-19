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



DATA_FNAME <- "intermediate_tanzania_for_R_1.dta"
VARIABLE_TABLE_FNAME <- "variable_table_tanzania.xlsx"

DATA_PATH <- paste(TARGETING_DATA_IN, DATA_FNAME, sep="/")
VARIABLE_TABLE_PATH <- paste(TARGETING_DATA_IN, VARIABLE_TABLE_FNAME, sep="/")

YEARS <- c("2008", "2010", "2012")
YEAR_IDS <- c("2008_hhid", "2010_hhid", "2012_hhid")
TARGET <- "lconsPC"

# Load data ---------------------------

load_data <- function() {
  read.dta(DATA_PATH)
}

add_covariates <- function(output_df, tanzania_panel) {
  feature_info <- read.xlsx(VARIABLE_TABLE_PATH, sheetName="Var (Final)")

  # Select features and determine feature types
  # Just using priority one variables for now
  is_priority_one <- feature_info$priority == 1
  exists_2008 <- feature_info$exists_2008_09 == 1
  exists_2010 <- feature_info$exists_2010_11 == 1
  exists_2012 <- feature_info$exists_2012_13 == 1
  exists_all <- exists_2008 & exists_2010 & exists_2012
  select_names_by_type <- function(type) {
    is_desired_type <- feature_info$type == type
    as.character(feature_info$var_name[is_priority_one & exists_all & is_desired_type])
  }
  
  covariates_categorical <- select_names_by_type("Categorical")
  covariates_cardinal <- select_names_by_type("Cardinal")
  covariates_yesno <- select_names_by_type("Yes/No")
  covariates <- c(covariates_categorical, covariates_cardinal, covariates_yesno)

  # Add features to output
  output_df[, covariates_categorical] <- tanzania_panel[, covariates_categorical]
  output_df[, covariates_cardinal] <- tanzania_panel[, covariates_cardinal]
  output_df[, covariates_yesno] <- tanzania_panel[, covariates_yesno]
  
  # Make sure features are cast correctly
  output_df[, covariates_categorical] <- lapply(output_df[, covariates_categorical], as.factor)
  output_df[, covariates_cardinal] <- lapply(output_df[, covariates_cardinal], as.numeric)
  # Note, may wish to recode some of the categoricals as ordered and the yes/no as categorical.
  output_df[, covariates_yesno] <- lapply(output_df[, covariates_yesno], as.factor)

  output_df
}

add_target <- function(output_df, panel_df) {
  output_df[, TARGET] <- log(panel_df$expmR / panel_df$hhsize) 
  output_df
}

remove_missing_data <- function(output_df) {
  output_df[complete.cases(output_df), ]
}

select_year <- function(output_df, panel_df, year) {
  output_df[panel_df$year == year, ]
}

create_dataset <- function() {
  tanzania_panel <- load_data()
  df <-
    matrix(nrow=nrow(tanzania_panel), ncol=0) %>%
    data.frame() %>%
    add_covariates(tanzania_panel) %>%
    add_target(tanzania_panel)
  df$locality <- as.character(df$locality)
  df$district <- as.character(df$district)
  df$year <- tanzania_panel$year
  df[, YEAR_IDS[1]] <- tanzania_panel$y1_hhid
  df[, YEAR_IDS[2]] <- tanzania_panel$y2_hhid
  df[, YEAR_IDS[3]] <- tanzania_panel$y3_hhid
  df
}

create_dataset_joined <- function(y1, y2, remove_missing=TRUE) {
  stopifnot(y1 <= y2)
  year1 <- YEARS[y1]
  year1_id <- YEAR_IDS[y1]
  year2 <- YEARS[y2]
  year2_id <- YEAR_IDS[y2]
  panel <- create_dataset()
  df1 <- filter(panel, year == year1) %>% select(-one_of(TARGET))
  df2 <- filter(panel, year == year2) %>% select(one_of(year1_id, TARGET))
  df <- merge(df1, df2, by.x=year1_id, by.y=year1_id) %>% select(-one_of("year")) %>% select(-contains("hhid"))
  if (remove_missing) df <- remove_missing_data(df)
  df
}

create_dataset_split <- function(y1, y2) {
  stopifnot(y1 <= y2)
  year1 <- YEARS[y1]
  year1_id <- YEAR_IDS[y1]
  year2 <- YEARS[y2]
  year2_id <- YEAR_IDS[y2]
  panel <- create_dataset()
  panel <- remove_missing_data(panel)
  
  clean <- panel %>% select(-one_of("year")) %>% select(-contains("hhid"))
  x <- model.matrix(lconsPC ~ .,  clean)
  x_nmm <- select(clean, -one_of(TARGET))
  y <- clean[rownames(x), TARGET]
  
  x_train <- x[panel$year == year1, ]
  x_train_nmm <- x_nmm[panel$year == year1, ]
  y_train <- y[panel$year == year1]
  
  x_test <- x[panel$year == year2, ]
  x_test_nmm <- x_nmm[panel$year == year2, ]
  y_test <- y[panel$year == year2]
  
  ksplit <- list(x_train=x_train, y_train=y_train, x_test=x_test, y_test=y_test)
  ksplit_nmm <- list(x_train=x_train_nmm, y_train=y_train, x_test=x_test_nmm, y_test=y_test)
  list(ksplit=list(splits=list(ksplit), assignments=1), ksplit_nmm=list(splits=list(ksplit_nmm), assignments=1))
}


# Run analysis ---------------------------


run_all <- function(name, df, ksplit, ksplit_nmm) {
  save_dataset(name, df)
  
  print("Running ridge")
  ridge <- kfold_(Ridge(), ksplit)
  print("Running lasso")
  lasso <- kfold_(Lasso(), ksplit)
  print("Running lasso 15")
  lasso_15 <- kfold_(Lasso(max_covariates=15), ksplit)
  print("Running least squares")
  least_squares <- kfold_(LeastSquares(), ksplit)
  
  print("Running stepwise")
  stepwise <- kfold_(Stepwise(300), ksplit)
  print("Running stepwise 15")
  stepwise_15 <- kfold_(Stepwise(15), ksplit)
  
  print("Running grouped ridge")
  ridge_district <- kfold_(GroupedRidge("district"), ksplit_nmm)
  ridge_locality <- kfold_(GroupedRidge("locality"), ksplit_nmm)
  
  print("Running rtree")
  rtree <- kfold_(rTree2(), ksplit_nmm)
  
  print("Running randomForest")
  forest <- kfold_(Forest(), ksplit)
  
#   print("Running mca")
#   mca_knn <- kfold_(MCA_KNN(ndim=12, k=5), ksplit_nmm)
  print("Running pca")
  pca_knn <- kfold_(PCA_KNN(ndim=12), ksplit_nmm)
  print("Running pca all")
  pca_knn_all <- kfold_(PCA_KNN(ndim=12), ksplit)
  
  
  threshold_20 <- quantile(df[, TARGET], .2, na.rm=TRUE)
  print("Running logistic")
  logistic_20 <- kfold_(Logistic(threshold_20), ksplit)
  print("Running logisitic lasso")
  logistic_lasso_20 <- kfold_(LogisticLasso(threshold_20), ksplit)
  print(" Running ctree")
  ctree_20 <- kfold_(cTree2(threshold_20), ksplit_nmm)
  print("Running randomForest")
  cforest_20 <- kfold_(cForest(threshold_20), ksplit)
  
  threshold_30 <- quantile(df[, TARGET], .3, na.rm=TRUE)
  print("Running logistic")
  logistic_30 <- kfold_(Logistic(threshold_30), ksplit)
  print("Running logisitic lasso")
  logistic_lasso_30 <- kfold_(LogisticLasso(threshold_30), ksplit)
  print(" Running ctree")
  ctree_30 <- kfold_(cTree2(threshold_30), ksplit_nmm)
  print("Running randomForest")
  cforest_30 <- kfold_(cForest(threshold_30), ksplit)
  
#   # Rerun with interaction terms
#   x_ix <- model.matrix(lconsPC ~ . + .:.,  df)
#   y_ix <- tz08[rownames(x_ix), "lconsPC"]
#   
#   print("Running ridge with interactions")
#   ridge_ix <- kfold_(k, Ridge(), y_ix, x_ix)
#   print("Running lasso with interactions")
#   lasso_ix <- kfold_(k, Lasso(), y_ix, x_ix)
#   print("Running least squares with interactions")
#   least_squares_ix <- kfold_(k, LeastSquares(), y_ix, x_ix)
#   print("Running logistic with interactions")
#   logistic_30_ix <- kfold_(k, Logistic(threshold_30), y_ix, xi_ix)
#   print("Running logisitic lasso with interactions")
#   logistic_lasso_30_ix <- kfold_(k, LogisticLasso(threshold_30), y_ix, x_ix)
  
  save_models(name,
              ridge=ridge,
              lasso=lasso,
              lasso_15=lasso_15,
              least_squares=least_squares,
              stepwise=stepwise,
              stepwise_15=stepwise_15,
              ridge_district=ridge_district,
              ridge_locality=ridge_locality,
              rtree=rtree,
              forest=forest,
              pca_knn=pca_knn,
              pca_knn_all=pca_knn_all,
              logistic_20=logistic_20,
              logistic_lasso_20=logistic_lasso_20,
              ctree_20=ctree_20,
              cforest_20=cforest_20,
              logistic_30=logistic_30,
              logistic_lasso_30=logistic_lasso_30,
              ctree_30=ctree_30,
              cforest_30=cforest_30)
}

tz08_10 <- create_dataset_joined(1, 1, remove_missing=TRUE)
tz08_10 <- standardize_predictors(tz08_10, TARGET)
x <- model.matrix(lconsPC ~ .,  tz08_10)
x_nmm <- select(tz08_10,-one_of(TARGET))
y <- tz08_10[rownames(x), TARGET]
k <- 4
ksplit <- kfold_split(k, y, x, seed=1)
ksplit_nmm <- kfold_split(k, y, x_nmm, seed=1)
run_all("tanzania_10_from_08_joined", tz08_10, ksplit, ksplit_nmm)

tz08_10_split <- create_dataset_split(1, 1)
panel <- create_dataset()
df1 <- filter(panel, year==YEARS[1])
ksplit <- tz08_10_split$ksplit
ksplit_nmm <- tz08_10_split$ksplit_nmm
run_all("tanzania_10_from_08_split", df1, ksplit, ksplit_nmm)
