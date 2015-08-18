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
  df$locality <- as.factor(as.character(df$locality))
  df$district <- as.factor(as.character(df$district))
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
  y <- clean[rownames(x), TARGET]
  
  x_train <- x[panel$year == year1, ]
  y_train <- y[panel$year == year1]
  w_train <- rep(1, length(y_train))
  
  
  x_test <- x[panel$year == year2, ]
  y_test <- y[panel$year == year2]
  
  holdouts <- as.logical(rbinom(length(y_test), 1, .8))
  x_holdout <- x_test[holdouts, ]
  y_holdout <- y_test[holdouts]
  ids <- 1:length(y_test)
  id_holdout <- ids[holdouts]
  id_sorted <- ids[!holdouts]
  x_test <- x_test[!holdouts, ]
  y_test <- y_test[!holdouts]
  w_test <- rep(1, length(y_test))

  
  splits <- list(
    list(
      x_train=x_train,
      y_train=y_train,
      w_train=w_train,
      x_test=x_test,
      y_test=y_test,
      w_test=w_test)
  )
  assignments <- rep(1, length(y_test))
  ksplit <- list(splits=splits, assignments=assignments, id_sorted=id_sorted)
  list(
    list(
      cv=ksplit,
      nocv=ksplit,
      x_holdout=x_holdout,
      y_holdout=y_holdout,
      id_holdout=id_holdout)
  )
  
}


# tz08_10 <- create_dataset_joined(1, 2, remove_missing=TRUE)
# tz08_10 <- standardize_predictors(tz08_10, TARGET)
# x <- model.matrix(lconsPC ~ .,  tz08_10)
# x_nmm <- select(tz08_10,-one_of(TARGET))
# y <- tz08_10[rownames(x), TARGET]
# cv_splits <- cv_split(y, x, k=5, inner_k=3, seed=1)
# run_all_heldout('tanzania_panel_08_10', tz08_10, "lconsPC", cv_splits)
# run_weighted_heldout('tanzania_panel_08_10', tz08_10, "lconsPC", cv_splits)

tz10_12 <- create_dataset_joined(2, 3, remove_missing=TRUE)
tz10_12 <- standardize_predictors(tz10_12, TARGET)
x <- model.matrix(lconsPC ~ .,  tz10_12)
y <- tz10_12[rownames(x), TARGET]
cv_splits <- cv_split(y, x, k=5, inner_k=3, seed=1)
run_all_heldout('tanzania_panel_10_12', tz10_12, "lconsPC", cv_splits)
# run_weighted_heldout('tanzania_panel_10_12', tz10_12, "lconsPC", cv_splits)

# cv_splits <- create_dataset_split(1, 2)
# panel <- create_dataset()
# df1 <- filter(panel, year==YEARS[1])
# run_all_heldout('tanzania_panel_split_08_10', df1, "lconsPC", cv_splits)
# run_weighted_heldout('tanzania_panel_split_08_10', df1, "lconsPC", cv_splits)
# 
# cv_splits <- create_dataset_split(2, 3)
# panel <- create_dataset()
# df1 <- filter(panel, year==YEARS[2])
# run_all_heldout('tanzania_panel_split_10_12', df1, "lconsPC", cv_splits)
# run_weighted_heldout('tanzania_panel_split_10_12', df1, "lconsPC", cv_splits)