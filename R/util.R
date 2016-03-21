#' @export

TARGETING_DATA_IN <- "inst/extdata"
TARGETING_DATA_OUT <- "data"

MODELS_BASE <- "cv_out.csv"
DATASET_BASE <- ".csv"
ENSEMBLE_BASE <- "ensemble.csv"
SET_ASIDE_FRACTION <- 0.2


save_models <- function(name, ...) {
  models <- list(...)
  joined <- join_dfs(models)
  fname <- paste(name, MODELS_BASE, sep="_")
  out_path <- paste(TARGETING_DATA_OUT, fname, sep="/")
  write.csv(joined, file=out_path)
}


save_models_ <- function(name, joined) {
  fname <- paste(name, MODELS_BASE, sep="_")
  out_path <- paste(TARGETING_DATA_OUT, fname, sep="/")
  write.csv(joined, file=out_path)
}

save_validation_models_ <- function(name, joined) {
  fname <- paste(name, 'validation', MODELS_BASE, sep="_")
  out_path <- paste(TARGETING_DATA_OUT, fname, sep="/")
  write.csv(joined, file=out_path)
}


save_ensemble <- function(name, e) {
  fname <- paste(name, ENSEMBLE_BASE, sep="_")
  out_path <- paste(TARGETING_DATA_OUT, fname, sep="/")
  coefs <- sapply(e$fits, coef)
  write.csv(coefs, file=out_path)
}

load_ensemble <- function(name) {
  fname <- paste(name, ENSEMBLE_BASE, sep="_")
  out_path <- paste(TARGETING_DATA_OUT, fname, sep="/")
  read.csv(out_path)  
}

load_models <- function(name) {
  fname <- paste(name, MODELS_BASE, sep="_")
  out_path <- paste(TARGETING_DATA_OUT, fname, sep="/")
  read.csv(out_path)
}

load_validation_models <- function(name) {
  fname <- paste(name, 'validation', MODELS_BASE, sep="_")
  out_path <- paste(TARGETING_DATA_OUT, fname, sep="/")
  read.csv(out_path)
}

save_dataset <- function(name, data) {
  fname <- paste(name, DATASET_BASE, sep="")
  dataset_path <- paste(TARGETING_DATA_OUT, fname, sep="/")
  write.csv(data, file=dataset_path)
}


load_dataset <- function(name) {
  fname <- paste(name, DATASET_BASE, sep="")
  dataset_path <- paste(TARGETING_DATA_OUT, fname, sep="/")
  read.csv(dataset_path)
}

set_aside_holdout <- function(NAME, df) {
  set.seed(1)
  set_aside <- as.logical(rbinom(nrow(df), 1, SET_ASIDE_FRACTION))
  fname <- paste(NAME, 'holdout_row_ids.csv', sep='_')
  fname <- paste(TARGETING_DATA_IN, fname, sep='/')
  write.csv(set_aside, fname)
  holdout <- df[set_aside, ]
  use <- df[!set_aside, ]
  use
}

config_path <- function(NAME) {
  fname <- paste(NAME, 'config.json', sep='_')
  paste(TARGETING_DATA_OUT, fname, sep='/')
}

save_config <- function(NAME, config) {
  fpath <- config_path(NAME)
  as_json <- jsonlite::toJSON(config)
  write(as_json, fpath)
}

read_config <- function(NAME) {
  fpath <- config_path(NAME)
  if (file.exists(fpath)) jsonlite::fromJSON(fpath) else list()
}

clear_config <- function(NAME) {
  fpath <- config_path(NAME)
  file.remove(fpath)
}

