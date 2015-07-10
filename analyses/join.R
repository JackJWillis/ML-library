library(ggplot2)
library(dplyr)
library(reshape)
paths <- commandArgs(TRUE)

df <- read.csv(paths[1])
df <- melt(df, id=c('method', 'fold'))
for (path in paths[-1]) {
  new_df <- read.csv(path)
  new_df <- melt(new_df, id=c('method', 'fold'))
  df <- rbind(df, new_df)
}
plot_scores <- function(df, fname) {
  ggplot(df, aes(y=value, x=method, color=variable, alpha=alpha)) + 
    geom_point(position=position_dodge(width=0.5)) +
    scale_fill_brewer(palette = "Set1") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(y = "reach - lms reach") +
    scale_alpha_continuous(range=c(0.3, 1), guide=FALSE)
  ggsave(fname, width=12, height=8)
}
df$alpha <- 1
plot_scores(df, 'results/scores_all.pdf')
df_avg <- group_by(df, method, variable) %>% summarize(value=mean(value))
df_avg$alpha <- 2
plot_scores(df_avg, 'results/scores_avg.pdf')
df$fold <- NULL
df <- rbind(df, df_avg)
plot_scores(df, 'results/scores.pdf')
write.csv(cast(df, method ~ variable, mean), 'results/scores.csv', row.names=FALSE)
