library(knitr)
library(MLlibrary)

args <- commandArgs(TRUE)
NAME <- args[1]
dataset <- load_dataset(NAME)
joined <- load_models(NAME)
path <- paste('results', NAME, sep='/')
knit2html('analyses/report.Rmd', output=path)
reach_df <- calculate_reach_(joined, base='least_squares')
names(reach_df)[names(reach_df) == 'reach'] <- NAME
write.csv(reach_df, paste(path, '.csv', sep=''), row.names=FALSE)
