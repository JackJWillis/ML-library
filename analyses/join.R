paths <- commandArgs(TRUE)

df <- read.csv(paths[1])
for (path in paths[-1]) {
  new_df <- read.csv(path)
  df <- merge(df,  new_df, by='method', all=TRUE)
}
write.csv(df, 'results/scores.csv', row.names=FALSE)
