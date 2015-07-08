library(knitr)
library(MLlibrary)

args <- commandArgs(TRUE)
NAME <- args[1]
dataset <- load_dataset(NAME)
joined <- load_models(NAME)
knit2html('analyses/report.Rmd', output=NAME)
reach_df <- calculate_reach_(joined, base='least_squares')
write.csv(reach_df, paste(NAME, '.csv', sep=''))
