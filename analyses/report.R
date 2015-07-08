library(knitr)
library(MLlibrary)

args <- commandArgs(TRUE)
NAME <- args[1]
knit2html('report.Rmd', output=NAME)
joined <- load <- models(NAME)
reach_df <- calculate_reach_(NAME)
write.csv(reach_df, paste(NAME, '.csv', sep=''))
