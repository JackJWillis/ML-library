
library(MLlibrary)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

#library(doMC)
#registerDoMC(cores=3)
K = 5



gaussian <- function(mean, sd) {
  function(response) {response + sd(response)*rnorm(length(response), mean=mean, sd=sd)}
}

gaussian_x <- function(mean, sd, random=F) {
  if (random) {
    function(response) {apply(response,2,gaussian(mean,sd=runif(1)*sd))}
  }
  else {
    function(response) {apply(response,2,gaussian(mean,sd))}
  }
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

cuts <- function(nvar=10, nrow=100, nest = F) {
  X <- matrix(rnorm(nvar * nrow), nrow=nrow, ncol=nvar)
  cuts <- rnorm(nvar)
  cuts <- matrix(cuts, nrow=nrow, ncol=nvar, byrow = TRUE)
  X_transformed <- X<cuts
  if (nest) {
    for (n in 1:20) {
      temp <- sample(1:nvar,2,replace=F)
      X_transformed[,temp[1]] <- X_transformed[,temp[1]]*X_transformed[,temp[2]]
    }
  }
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
  X_ix <- model.matrix(y ~ . + .:.,  data.frame(dataset))
  y <- dataset$y
  ksplit <- kfold_split(K, y, X, seed=1)
  ksplit_nmm <- kfold_split(K, y, data.frame(X), seed=1)
  ksplit_ix <- kfold_split(K, y, X_ix, seed=1)
  dataset <- data.frame(y=y,X)
  run_simulation_models(NAME, dataset, "y", ksplit, ksplit_nmm, ksplit_ix)
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
  reach_df <- calculate_reach_(joined, fold=TRUE)
  reach_df$name <- NAME
  reach_df
}


plot_scores_OLD <- function(df, fname) {
  df$type[df$method %in% c("least_squares","stepwise","stepwise_15","ridge","lasso", "lasso_15","ridge")] <- "regularization"
  df$type[df$method %in% c("pca_knn","pca_knn_all","forest","btree","rtree")] <- "non-linear"
  df$type[df$method %in% c("logistic_40","logistic_lasso_40","ctree_40","cforest_40","cbtree_40","ctree_40","quantile")] <- "different loss"
  df<-df[!is.na(df$type),]
  ggplot(df, aes(y=reach, x=method, color=name)) + 
      geom_point(position=position_dodge(width=0.5)) +
      scale_colour_brewer(palette="RdYlBu") + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(y = "reach") +
      facet_wrap( ~ type, ncol=3)
  ggsave(fname, width=12, height=8)
}

plot_scores <- function(df, fname) {
  
  names <- c("least_squares")
  levels(df$method)[levels(df$method) %in% names] <- sapply(names,function(response) {paste("0.",response,sep=" -- ")})
  names <- c("stepwise","stepwise_15","ridge","lasso", "lasso_15","ridge")
  levels(df$method)[levels(df$method) %in% names] <- sapply(names,function(response) {paste("1. Linear regularized",response,sep=" -- ")})
  names <- c("ridge_ix","lasso_ix", "least_squares_ix")
  levels(df$method)[levels(df$method) %in% names] <- sapply(names,function(response) {paste("2. Linear interactions",response,sep=" -- ")})
  names <- c("pca_knn","pca_knn_all","forest","btree","rtree")  
  levels(df$method)[levels(df$method) %in% names] <- sapply(names,function(response) {paste("3. Non-linear regression",response,sep=" -- ")})
  names <- c("logistic_40","logistic_lasso_40","ctree_40","cforest_40","cbtree_40","ctree_40")
  levels(df$method)[levels(df$method) %in% names] <- sapply(names,function(response) {paste("4. Classification",response,sep=" -- ")})
  df$method <- as.factor(as.character(df$method))
  ggplot(df, aes(y=reach, x=method, color=name)) + 
    geom_point(position=position_dodge(width=0.5)) +
    scale_colour_brewer(palette="RdYlBu") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(y = "reach") +
    geom_vline(xintercept = 1.5) +
    geom_vline(xintercept = 6.5) +
    geom_vline(xintercept = 9.5) +
    geom_vline(xintercept = 14.5) +
    geom_vline(xintercept = 19.5) +
    geom_hline(yintercept = 0.16)
  ggsave(fname, width=12, height=8)
}


##Linear y noise

NAME <- "linear"
dataset <- add_noise(linear(nvar=20, nrow=1000))
temp_df <- run_dataset(dataset, NAME)
reach_df <- temp_df
for (j in c(0.1,0.2,0.5,1,2,5)) {
  NAME <- paste('linear', j, sep='_')
  noise_y <- gaussian(0,j)
  noise_x <- gaussian_x(0,0)
  dataset <- add_noise(linear(nvar=20, nrow=1000), noise_y=noise_y, noise_x=noise_x)
  temp_df <- run_dataset(dataset, NAME)
  reach_df <- rbind(reach_df,temp_df)    
} 
reach_df_avg <- group_by(reach_df, method, name) %>% summarize(reach=mean(reach))
plot_scores(reach_df_avg, 'results/linear_y_scores_avg.pdf')

##Linear x noise

NAME <- "linear"
dataset <- add_noise(linear(nvar=20, nrow=1000))
temp_df <- run_dataset(dataset, NAME)
reach_df <- temp_df
for (i in c(0.1,0.2,0.5,1,2,5)) {
  NAME <- paste('linear', i, sep='_')
  noise_y <- gaussian(0,0)
  noise_x <- gaussian_x(0,i,random=T)
  dataset <- add_noise(linear(nvar=20, nrow=1000), noise_y=noise_y, noise_x=noise_x)
  temp_df <- run_dataset(dataset, NAME)
  reach_df <- rbind(reach_df,temp_df)    
} 
reach_df_avg <- group_by(reach_df, method, name) %>% summarize(reach=mean(reach))
plot_scores(reach_df_avg, 'results/linear_x_scores_avg.pdf')


##Cuts y noise

NAME <- "cuts"
dataset <- add_noise(cuts(nvar=20, nrow=1000, nest=T))
temp_df <- run_dataset(dataset, NAME)
reach_df <- temp_df
for (j in c(0.1,0.2,0.5,1,2,5)) {
  NAME <- paste('cuts', j, sep='_')
  noise_y <- gaussian(0,j)
  noise_x <- gaussian_x(0,0)
  dataset <- add_noise(cuts(nvar=20, nrow=1000), noise_y=noise_y, noise_x=noise_x)
  temp_df <- run_dataset(dataset, NAME)
  reach_df <- rbind(reach_df,temp_df)    
} 
reach_df_avg <- group_by(reach_df, method, name) %>% summarize(reach=mean(reach))
plot_scores(reach_df_avg, 'results/cuts_y_scores_avg.pdf')

##Cuts x noise

NAME <- "cuts"
dataset <- add_noise(cuts(nvar=20, nrow=1000, nest=T))
temp_df <- run_dataset(dataset, NAME)
reach_df <- temp_df
for (i in c(0.1,0.2,0.5,1,2,5)) {
  NAME <- paste('cuts', i, sep='_')
  noise_y <- gaussian(0,0)
  noise_x <- gaussian_x(0,i,random=T)
  dataset <- add_noise(cuts(nvar=20, nrow=1000), noise_y=noise_y, noise_x=noise_x)
  temp_df <- run_dataset(dataset, NAME)
  reach_df <- rbind(reach_df,temp_df)    
} 
reach_df_avg <- group_by(reach_df, method, name) %>% summarize(reach=mean(reach))
plot_scores(reach_df_avg, 'results/cuts_x_scores_avg.pdf')
