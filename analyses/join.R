library(ggplot2)
library(dplyr)
library(reshape)
library(magrittr)
paths <- commandArgs(TRUE)

extension <- tail(strsplit(paths[1], '_')[[1]], n=1)
metric <- strsplit(extension, '[.]')[[1]][[1]]
df <- read.csv(paths[1])
id <- 'method'
if ('fold' %in% names(df)) {
  id <- c(id, 'fold')
}
df <- melt(df, id=id)
for (path in paths[-1]) {
  new_df <- read.csv(path)
  new_df <- melt(new_df, id=id)
  df <- rbind(df, new_df)
}

plot_scores <- function(df, fname) {
  df$method <- as.factor(as.character(df$method))
  reg_names <- c("stepwise","stepwise_15", "lasso", "lasso_15", "ridge", "least_squares_pc")
  nonlin_names <- c("pca_knn","pca_knn_all","forest","btree","btree_laplace", "rtree")  
  class_names <- c("logistic_40","logistic_lasso_40","ctree_40","cforest_40", "cbtree_40")
  ensemble_names <- c("ensemble", "ensemble_all")
  all_names <- c(reg_names, nonlin_names, class_names, ensemble_names)
  df <- filter(df, method %in% all_names)
  
  levels(df$method)[levels(df$method) %in% reg_names] <- sapply(reg_names,function(response) {paste("1. Linear regularized",response,sep=" -- ")})
  levels(df$method)[levels(df$method) %in% nonlin_names] <- sapply(nonlin_names,function(response) {paste("3. Non-linear regression",response,sep=" -- ")})
  levels(df$method)[levels(df$method) %in% class_names] <- sapply(class_names,function(response) {paste("4. Classification",response,sep=" -- ")})
  levels(df$method)[levels(df$method) %in% ensemble_names] <- sapply(ensemble_names,function(response) {paste("5. Ensemble",response,sep=" -- ")})
  df$method <- as.factor(as.character(df$method))
  p <- ggplot(df, aes(y=value, x=method, color=variable, alpha=alpha)) + 
    geom_point(position=position_dodge(width=0.5)) +
    scale_fill_brewer(palette = "Set1") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(y = paste(metric, "- lms", metric)) +
    scale_alpha_continuous(range=c(0.3, 1), guide=FALSE) +
    geom_vline(xintercept = 6.5) +
    geom_vline(xintercept = 12.5) +
    geom_vline(xintercept = 17.5)
  ggsave(fname, p, width=12, height=8)
  
}
df$alpha <- 1
plot_scores(df, paste('results/', metric, '_all.pdf', sep=''))
if (metric == "reach") {
  df_avg <- group_by(df, method, variable) %>% summarize(value=mean(value))
  df_avg$alpha <- 2
  plot_scores(df_avg, paste('results/', metric, '_avg.pdf', sep=''))
  df$fold <- NULL
  df <- rbind(df, df_avg)
  plot_scores(df, paste('results/', metric, '.pdf', sep=''))
  write.csv(cast(df, method ~ variable, mean),  paste('results/', metric, '.csv', sep=''), row.names=FALSE)
}
