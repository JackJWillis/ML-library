
library(MLlibrary)
library(ggplot2)
library(dplyr)
#library(doMC)
#registerDoMC(cores=3)
K = 5



gaussian <- function(mean, sd) {
  function(response) {response + sd(response)*rnorm(length(response), mean=mean, sd=sd)}
}

gaussian_x <- function(mean, sd) {
  function(response) {apply(response,2,gaussian(mean,sd))}
}

add_noise <-function(d, noise_y=function(response) {response}, noise_x=function(response) {response}) {
  d$y <- noise_y(d$y)
  d$X <- noise_x(d$X)
  d
}

linear <- function(nvar=10, nrow=100) {
  variables <- as.character(seq_len(nvar))
  coefficients <- seq_along(variables)
  X <- matrix(rnorm(nvar * nrow), nrow=nrow, ncol=nvar)
  y <- (X %*% coefficients) + 10
  list(y=y, X=X)
}

exponential <- function(nvar=10, nrow=100) {
  d <- linear_(nvar, nrow)
  d$y <- exp(d$y / 20)
  d
}

cuts <- function(nvar=10, nrow=100) {
  X <- matrix(rnorm(nvar * nrow), nrow=nrow, ncol=nvar)
  cuts <- rnorm(nvar)
  cuts <- matrix(cuts, nrow=nrow, ncol=nvar, byrow = TRUE)
  X_transformed <- X<cuts
  variables <- as.character(seq_len(nvar))
  coefficients <- seq_along(variables)
  y <- as.vector((X_transformed %*% coefficients) + 10)
  list(y=y, X=X)
}

cuts_and_linear <- function(nvar=10, nrow=100, alpha=0.5) {
  X <- matrix(rnorm(nvar * nrow), nrow=nrow, ncol=nvar)
  cuts <- rnorm(nvar)
  cuts <- matrix(cuts, nrow=nrow, ncol=nvar, byrow = TRUE)
  X_transformed <- X<cuts
  variables <- as.character(seq_len(nvar))
  coefficients <- seq_along(variables)
  y <- (alpha*as.vector((X_transformed %*% coefficients) + (1-alpha)*(X %*% coefficients))) + 10
  list(y=y, X=X)
}





run_all <- function(dataset,NAME) {
  X <- dataset$X
  y <- dataset$y
  ksplit <- kfold_split(K, y, X, seed=1)
  ksplit_nmm <- kfold_split(K, y, data.frame(X), seed=1)
  dataset <- data.frame(y=y,X)
  run_all_models(NAME, dataset, "y", ksplit, ksplit_nmm)
}

plot_all <- function(NAME) {
  all_models <- load_models(NAME)
  plot_swf_(all_models)
  plot_reach_vs_waste_(all_models)
  plot_reach_vs_waste_(all_models, THRESHOLD=.4)
}

run_dataset <- function(dataset, NAME)  { 
  run_all(dataset, NAME)
  joined <- load_models(NAME)
  reach_df <- calculate_reach_(joined, fold=TRUE, base='least_squares')
  reach_df$name <- NAME
  reach_df
}


plot_scores <- function(df, fname) {
  df$type[df$method %in% c("true","stepwise","stepwise_15","rigde","lasso", "lasso_15")] <- "regularization"
  df$type[df$method %in% c("pca_knn","pca_knn_all","forest","btree","rtree","btree_laplace")] <- "non-linear"
  df$type[df$method %in% c("logistic_40","logistic_lasso_40","ctree_40","cforest_40","ctree_40")] <- "different loss"
  df<-df[!is.na(df$type),]
  ggplot(df, aes(y=reach, x=method, color=name, alpha=2)) + 
      geom_point(position=position_dodge(width=0.5)) +
      scale_fill_brewer(palette = "Set1") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(y = "reach - lms reach") +
      scale_alpha_continuous(range=c(0.3, 1), guide=FALSE) +
      facet_wrap( ~ type, ncol=3)
  ggsave(fname, width=12, height=8)
}

NAME <- "linear"
dataset <- add_noise(linear(nvar=10, nrow=100))
temp_df <- run_dataset(dataset, NAME)
reach_df <- temp_df

for (i in c(0.2,0.5,1)) {
  for (j in c(0.2,0.5,1)) {
    NAME <- paste('linear', i, j, sep='_')
    noise_y <- gaussian(0,j)
    noise_x <- gaussian_x(0,i)
    dataset <- add_noise(linear(nvar=10, nrow=100), noise_y=noise_y, noise_x=noise_x)
    temp_df <- run_dataset(dataset, NAME)
    reach_df <- rbind(reach_df,temp_df)    
  } 
}

reach_df_avg <- group_by(reach_df, method, name) %>% summarize(reach=mean(reach))
plot_scores(reach_df_avg, 'results/linear_scores_avg.pdf')

NAME <- "cuts"
dataset <- add_noise(cuts(nvar=10, nrow=100))
temp_df <- run_dataset(dataset, NAME)
reach_df <- temp_df

for (i in c(0.2,0.5,1)) {
  for (j in c(0.2,0.5,1)) {
    NAME <- paste('cuts', i, j, sep='_')
    noise_y <- gaussian(0,j)
    noise_x <- gaussian_x(0,i)
    dataset <- add_noise(cuts(nvar=10, nrow=100), noise_y=noise_y, noise_x=noise_x)
    temp_df <- run_dataset(dataset, NAME)
    reach_df <- rbind(reach_df,temp_df)    
  } 
}

#plot_scores(reach_df, 'results/simulation_scores_all.pdf')
reach_df_avg <- group_by(reach_df, method, name) %>% summarize(reach=mean(reach))
  plot_scores(reach_df_avg, 'results/cuts_scores_avg.pdf')


