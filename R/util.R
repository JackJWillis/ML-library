#' @export

TARGETING_DATA_IN <- "inst/extdata"
TARGETING_DATA_OUT <- "data"

MODELS_BASE <- "cv_out.csv"
DATASET_BASE <- ".csv"
ENSEMBLE_BASE <- "ensemble.csv"


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

