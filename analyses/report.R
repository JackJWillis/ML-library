library(knitr)

args <- commandArgs(TRUE)
NAME <- args[1]
knit2html('report.Rmd')
