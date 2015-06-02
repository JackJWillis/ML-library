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

NAME <- "mexico"

# Load data ---------------------------

consumption_variable <- "gastot"
hhsize_variable <- "tam_hog"

remove_missing_data <- function(output_df) {
  output_df[complete.cases(output_df), ]
}

create_dataset <- function(remove_missing=TRUE) {
  consumption <- read.dta(CONSUMPTION_PATH)
  households <- read.dta(HOUSEHOLDS_PATH)
  mexico <- merge(households, consumption[, c("folioviv", consumption_variable, hhsize_variable)], by="folioviv")
  df <- mexico %>%
    select(-contains("gas")) %>%
    select(-contains("folio")) %>%
    select(-contains("ubica_geo")) %>%
    select(-one_of("upm", "est_dis"))
    
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

print("Running ridge")
ridge <- kfold(k, Ridge(), y, x)
print("Running lasso")
lasso <- kfold(k, Lasso(), y, x)
print("Running least squares")
least_squares <- kfold(k, LeastSquares(), y, x)

print("Running rtree")
rtree <- kfold(k, rTree2(), y, x_nmm)
print("Running randomForest")
forest <- kfold(k, Forest(), y, x_nmm)

# Rerun with interaction terms
x_ix <- model.matrix(lnwelfare ~ . + .:.,  gh)
y_ix <- gh[rownames(x_ix), "lnwelfare"]

print("Running ridge with interactions")
ridge_ix <- kfold(k, Ridge(), y_ix, x_ix)
print("Running lasso with interactions")
lasso_ix <- kfold(k, Lasso(), y_ix, x_ix)
print("Running least squares with interactions")
least_squares_ix <- kfold(k, LeastSquares(), y_ix, x_ix)

save_models(NAME,
            ridge=ridge,
            lasso=lasso,
            least_squares=least_squares,
            rtree=rtree,
            forest=forest,
            ridge_ix=ridge_ix,
            lasso_ix=lasso_ix,
            least_squares_ix=least_squares_ix)
