library(ggplot2)
library(dplyr)
library(knitr)
library(MLlibrary)

paths <- commandArgs(TRUE)
reach_path <- paths[[1]]
ensemble_bttp_path <- paths[[2]]
joineds <- lapply(paths[c(-1, -2)], load_models)
method_names <- paths[c(-1, -2)]
knit2html('analyses/ensemble.Rmd', output='results/ensemble.html')
