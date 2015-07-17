library(knitr)
library(MLlibrary)

args <- commandArgs(TRUE)
NAME <- args[1]
dataset <- load_dataset(NAME)
joined <- load_models(NAME)
path <- paste('results', NAME, sep='/')
knit2html('analyses/report.Rmd', output=path)
reach_df <- calculate_reach_(joined, fold=TRUE, base='least_squares')
names(reach_df)[names(reach_df) == 'reach'] <- NAME
write.csv(reach_df, paste(path, '_reach', '.csv', sep=''), row.names=FALSE)

mse_df <- joined %>%
  filter(predicted > 2) %>%
  mutate(se=(true - predicted)^2) %>%
  group_by(method) %>%
  summarise(mse=mean(se))
ls <- filter(mse_df, method=="least_squares")
mse_df$mse <- ls$mse - mse_df$mse
mse_df <- filter(mse_df, method != "least_squares")
names(mse_df)[names(mse_df) == 'mse'] <- NAME
write.csv(mse_df, paste(path, '_mse', '.csv', sep=''), row.names=FALSE)

l1_df <- joined %>%
  filter(predicted > 2) %>%
  mutate(l1=abs(true - predicted)) %>%
  group_by(method) %>%
  summarise(l1=mean(l1))
ls <- filter(l1_df, method=="least_squares")
l1_df$l1 <-  ls$l1 - l1_df$l1
l1_df <- filter(l1_df, method != "least_squares")
names(l1_df)[names(l1_df) == 'l1'] <- NAME
write.csv(l1_df, paste(path, '_l1', '.csv', sep=''), row.names=FALSE)