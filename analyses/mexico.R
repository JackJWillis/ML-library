library(magrittr)
library(foreign)
library(xlsx)
library(dplyr)
library(MLlibrary)


CONSUMPTION <- "Concen.dta"
CONSUMPTION_PATH <- paste(TARGETING_DATA_IN, CONSUMPTION, sep="/")

INDIVIDUALS <- "Pobla10.dta"
INDIVIDUALS_PATH<- paste(TARGETING_DATA_IN, INDIVIDUALS, sep="/")

HOUSEHOLDS <- "Hogares.dta"
HOUSEHOLDS_PATH <- paste(TARGETING_DATA_IN, HOUSEHOLDS, sep="/")

VARIABLE_TABLE <- "varlist_Hogares.xlsx"
VARIABLE_TABLE_PATH <- paste(TARGETING_DATA_IN, VARIABLE_TABLE, sep="/")

NAME <- "mexico"

# Load data ---------------------------

consumption_variable <- "gastot"
hhsize_variable <- "tam_hog"

remove_missing_data <- function(output_df) {
  output_df[complete.cases(output_df), ]
}

transform_years <- function(years) {
  sapply(years, function(year_string) {
    year_num <- as.numeric(year_string)
    if (is.na(year_num) | year_num == -1) {
      NA
    }
    else {
      if (year_num < 15) {
        15 - year_num
      }
      else {
        115 - year_num
      }
    }
  })
}

cast_covariates <- function(output_df) {
  feature_info <- read.xlsx(VARIABLE_TABLE_PATH, sheetName="Sheet1", stringsAsFactors=FALSE)
  feature_info <- feature_info[2:nrow(feature_info), ]
  names(feature_info)[[1]] <- "variable_name"
  names(feature_info)[[2]] <- "storage_type"
  names(feature_info)[[3]] <- "display_format"
  names(feature_info)[[4]] <- "variable_label"
  
  covariates_categorical <- filter(feature_info, grepl("str", storage_type))$variable_name
  covariates_cardinal_or_ordinal <- filter(feature_info, grepl("int", storage_type) | grepl("byte", storage_type) | grepl("long", storage_type))$variable_name
  covariates_real <- filter(feature_info, grepl("float", storage_type) | grepl("double", storage_type))$variable_name
  
  output_df[, covariates_categorical] <- lapply(output_df[, covariates_categorical], as.factor)
  output_df[, covariates_cardinal_or_ordinal] <- lapply(output_df[, covariates_cardinal_or_ordinal], as.numeric)
  output_df[, covariates_real] <- lapply(output_df[, covariates_real], as.numeric)
  
  output_df
}

create_dataset <- function(remove_missing=TRUE) {
  consumption <- read.dta(CONSUMPTION_PATH)
  households <- read.dta(HOUSEHOLDS_PATH)
  households <- cast_covariates(households)
  mexico <- merge(households, consumption[, c("folioviv", consumption_variable, hhsize_variable)], by="folioviv")
  df <- mexico %>%
    select(-contains("gas")) %>%
    select(-contains("folio")) %>%
    select(-contains("ubica_geo")) %>%
    select(-one_of("upm", "est_dis")) %>%
    select(-one_of("renta", "renta_tri")) %>%
    select(-one_of("estim", "estim_tri")) %>%
    select(-one_of("pagoviv")) %>%
    select(-contains("tenen_")) %>%
    mutate(tam_loc=iconv(tam_loc, to="ascii", sub=""))
    
  
  # If these are factors, MCA does not work...
  df$state <- sapply(mexico$ubica_geo, function(s) substr(s, 1, 2))
  df$muni <- sapply(mexico$ubica_geo, function(s) substr(s, 3, 5))
  year_df <- select(df, ends_with("_a"))
  year_df <- lapply(year_df, transform_years)
  df[, names(year_df)] <- year_df
  df$lconsPC <- log(mexico[, consumption_variable] / mexico[, hhsize_variable])
  if (remove_missing) df <- remove_missing_data(df)
  df
}

# Run analysis ---------------------------

mx <- create_dataset(remove_missing=FALSE)
mx <- na_indicator(mx)
mx <- standardize_predictors(mx, "lconsPC")
save_dataset(NAME, mx)
x <- model.matrix(lconsPC ~ .,  mx)
x_nmm <- select(mx, -one_of("lconsPC"))
y <- mx[rownames(x), "lconsPC"]
k <- 5

# print("Running ridge")
# ridge <- kfold(k, Ridge(), y, x)
# print("Running lasso")
# lasso <- kfold(k, Lasso(), y, x)
# print("Running least squares")
# least_squares <- kfold(k, LeastSquares(), y, x)

# print("Running rtree")
# rtree <- kfold(k, rTree2(), y, x_nmm)
# print("Running randomForest")
# forest <- kfold(k, Forest(), y, x_nmm)

# # Rerun with interaction terms
# x_ix <- model.matrix(lnwelfare ~ . + .:.,  gh)
# y_ix <- gh[rownames(x_ix), "lnwelfare"]

# print("Running ridge with interactions")
# ridge_ix <- kfold(k, Ridge(), y_ix, x_ix)
# print("Running lasso with interactions")
# lasso_ix <- kfold(k, Lasso(), y_ix, x_ix)
# print("Running least squares with interactions")
# least_squares_ix <- kfold(k, LeastSquares(), y_ix, x_ix)

# save_models(NAME,
#             ridge=ridge,
#             lasso=lasso,
#             least_squares=least_squares,
#             rtree=rtree,
#             forest=forest,
#             ridge_ix=ridge_ix,
#             lasso_ix=lasso_ix,
#             least_squares_ix=least_squares_ix)
