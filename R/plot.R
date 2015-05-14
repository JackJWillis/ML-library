#############
### This file contains graphing functions for plotting the output from the ML_functions
#############

check_ids_match <- function(dfs) {
  ids <- lapply(dfs, function(df) df$id)
  all_equal <- lapply(ids, function(id_list) id_list == ids[[1]])
  all(unlist(all_equal))
}

check_trues_match <- function(dfs) {
  trues <- lapply(dfs, function(df) df$true)
  all_equal <- lapply(trues, function(trues_list) all.equal(trues_list, trues[[1]]))
  all(unlist(all_equal))
}

join_dfs <- function(dfs, keep_fold=FALSE) {
  if(length(dfs) != 1) {
    stopifnot(check_ids_match(dfs))
    stopifnot(check_trues_match(dfs))
  }
  if(is.null(names(dfs))) {
    names(dfs) <- seq_len(length(dfs))
  }
  df <- data.frame(matrix(nrow=nrow(dfs[[1]]), ncol=0))
  df$id <- dfs[[1]]$id
  df$true <- dfs[[1]]$true
  
  rename_columns <- function(name, df) {
    new_df <- data.frame(matrix(nrow=nrow(df), ncol=0))
    new_df[, name] <- df$predicted
    if(keep_fold) {
      new_df[, paste(name, "fold", sep="_")] <- df$fold
    }
    new_df[, "id"] <- df$id
    new_df
  }

  join_two <- function(df1, df2) {
    merge(df1, df2, by="id")
  }

  renamed_dfs <- mapply(rename_columns, names(dfs), dfs, SIMPLIFY=FALSE)
  Reduce(join_two, renamed_dfs, init=df)
}


plot_scatter <- function(...) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  melted <- reshape2::melt(joined, value.name="predicted", variable.name="method", id=c("id", "true"))
  ggplot2::ggplot(melted, ggplot2::aes(x=true, y=predicted, color=method)) +
    ggplot2::geom_point(alpha=0.5)
}

plot_density<- function(...) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  melted <- reshape2::melt(joined, variable.name="method", id=c("id"))
  ggplot2::ggplot(melted, ggplot2::aes(x=value, fill=method)) +
    ggplot2::geom_density(alpha=0.5)
}

#' Produce an ROC curve which plots a given method's sensitivity/specificity with respect
#' a given poverty threshold.
plot_roc <- function(THRESHOLD, ...) {
  dfs <- list(...)
  joined <- join_dfs(dfs)
  joined$response <- joined$true < THRESHOLD
  joined$id <- NULL
  joined$true <- NULL
  
  roc_formula <- as.formula(paste("response ~", paste(names(dfs), collapse="+")))
  rocs <- pROC::roc(roc_formula, data=joined, plot=FALSE)
  if(length(dfs) == 1) {
    rocs <- list(rocs)
    names(rocs) <- names(dfs)
  }
  roc_to_df <- function(name, roc) {
    data.frame(sensitivity=roc$sensitivities, specificity=roc$specificities, method=name)
  }
  roc_dfs <- mapply(roc_to_df, names(rocs), rocs, SIMPLIFY=FALSE)
  roc_df <- do.call("rbind", roc_dfs)
  ggplot2::ggplot(roc_df, ggplot2::aes(x=specificity, y=sensitivity, color=method)) +
    ggplot2::geom_step() +
    ggplot2::scale_x_reverse() +
    ggplot2::geom_abline(intercept=1, slope=1, alpha=0.5) 
  # TODO display auc
}


#' If we target N people, what fraction of the true poor would receive funds?
#' True Positives / Total Positives
plot_accuracy <- function(THRESHOLD, ...) {
  POINT_COUNT = 20
  dfs <- list(...)
  joined <- join_dfs(dfs)
  N <- nrow(joined)
  plot_points <- seq(1, N, length=POINT_COUNT)
  joined$response <- joined$true < THRESHOLD
  true_poor <- sum(joined$response)

  get_coverage <- function(method, df) {
    cumsum(df[order(df[, method]), "response"]) / true_poor
  }
  ranked <- data.frame(mapply(get_cumsum, names(dfs), list(joined), SIMPLIFY=FALSE))
  
  cut <- ranked[plot_points, ,drop=FALSE]
  cut$percent_population_included <- plot_points / N
  melted <- reshape2::melt(
    cut, 
    value.name="coverage",
    variable.name="method",
    id="percent_population_included")
  ggplot2::ggplot(melted, ggplot2::aes(x=percent_population_included, y=coverage, color=method)) +
    ggplot2::geom_step() +
    ggplot2::geom_point()
}


#' With a fixed amount of money, if we target N people, what fraction would go to the true poor?
#' True Positives / (True Positives + False Positives)
plot_accuracy_dollars <- function(THRESHOLD, ...) {
  POINT_COUNT <- 20
  dfs <- list(...)
  joined <- join_dfs(dfs)
  N <- nrow(joined)
  plot_points <- seq(1, N, length=POINT_COUNT)
  joined$response <- joined$true < THRESHOLD

  get_to_true_poor <- function(method, df) {
    cumsum(df[order(df[, method]), "response"]) / seq(1, N)
  }
  ranked <- data.frame(mapply(get_to_true_poor, names(dfs), list(joined), SIMPLIFY=FALSE))
  
  cut <- ranked[plot_points, , drop=FALSE]
  cut$percent_population_included <- plot_points / N
  melted <- reshape2::melt(
    cut, 
    value.name="to_true_poor",
    variable.name="method",
    id="percent_population_included")
  ggplot2::ggplot(melted, ggplot2::aes(x=percent_population_included, y=to_true_poor, color=method)) +
    ggplot2::geom_step() +
    ggplot2::geom_point()
}




###Scoring curve

scoring.curve.1.partialplot =function(out,j,s){
  nobs=length(out$y_pred)
  sMatrix = matrix(s,nrow=nobs,ncol=length(s),byrow=TRUE)
  #tempReal has S columns, each one a copy of Yreal
  tempReal = matrix(out$y_real,nrow=nobs,ncol=ncol(sMatrix))
  x <- vector(length = ncol(sMatrix))
  #x is proportion actually poor under s
  x = as.vector(colSums(tempReal < sMatrix)/nobs)   
  temp = matrix(out$y_pred,nrow=nobs,ncol=ncol(sMatrix)) 
  y <- vector(length = ncol(sMatrix))
  #y is proprtion of those deemed eligible who are actually poor at s
  y = as.vector(colSums(1*((temp < sMatrix)*(tempReal < sMatrix)))/colSums(1*(temp < sMatrix)))
  if (j==1)  {
    plot(x,y,col="grey",lwd=2,type="l")
  }
  else {
    lines(x,y,type="l",col="grey",lwd=2)
  }
}

scoring.curve.1.kfold = function(kfold){
  process.one <- function(onefold) {
    y_min = min(onefold$out$y_real)
    y_max = max(onefold$out$y_real)
    c(y_min,y_max)
  }
  temp <- sapply(kfold,process.one)
  y_min <- min(temp)
  y_max <- max(temp)
  #S is range of possible values
  s=seq(y_min,y_max,by=0.1)
  for(j in 1:10){
    scoring.curve.1.partialplot(kfold[[j]]$out,j,s)   
  }
}

### ROC curves

ROC.curve.1.partialplot =function(out,j,s,cutoff,col="grey"){
  nobs=length(out$y_pred)
  sMatrix = matrix(s,nrow=nobs,ncol=length(s),byrow=TRUE)
  #tempReal has S columns, each one a copy of Yreal
  tempReal = matrix(out$y_real,nrow=nobs,ncol=ncol(sMatrix))
  tempPred = matrix(out$y_pred,nrow=nobs,ncol=ncol(sMatrix)) 
  FPmatrix = (tempPred < sMatrix)*(tempReal>cutoff)
  TPmatrix = (tempPred < sMatrix)*(tempReal<cutoff)
  #x is proportion actually poor under s
  FPR = as.vector(colSums(FPmatrix)/colSums(tempReal>cutoff))   
  TPR = as.vector(colSums(TPmatrix)/colSums(tempReal<cutoff))  
  if (j==1)  {
    plot(FPR,TPR,col=col,lwd=2,type="l")
  }
  else {
    lines(FPR,TPR,type="l",col=col,lwd=2)
  }
}

ROC.curve.1.kfold = function(kfold,cutoff,only_lines = FALSE,col="grey"){
  process.one <- function(onefold) {
    y_min = min(onefold$out$y_real)
    y_max = max(onefold$out$y_real)
    c(y_min,y_max)
  }
  temp <- sapply(kfold,process.one)
  y_min <- min(temp)
  y_max <- max(temp)
  #S is range of possible values
  s=seq(y_min,y_max,by=0.1)
  for(j in 1:10){
    ROC.curve.1.partialplot(kfold[[j]]$out,j+only_lines,s,cutoff,col)   
  }
}







#####OLD
#Scoring curve 1 assumes that the original threshold is stuck to when distributing funds
scoring.curve.1 =function(Yreal,Ypre){
  npred=ncol(Ypred)
  nobs=nrow(Ypred)
  y_min = min(Yreal)
  y_max = max(Yreal)
  #S is range of possible values
  s=seq(y_min,y_max,by=0.1)
  x <- vector(length = length(s))
  y <- matrix(nrow = length(s), ncol = npred)
  #Improve the coding of this later
  #sMatrix has each row as S, one ro for each obs
  sMatrix = matrix(s,nrow=nobs,length(s),byrow=TRUE)
  #tempReal has S columns, each one a copy of Yreal
  tempReal = matrix(Yreal,nrow=nobs,ncol=length(s))
  x = as.vector(colSums(tempReal < sMatrix)/nobs)   
  for(j in 1:npred){
    temp = matrix(Ypred[,j],nrow=nobs,ncol=length(s)) 
    y[,j] = as.vector(colSums(1*((temp < sMatrix)*(tempReal < sMatrix)))/colSums(1*(temp < sMatrix)))   
  }
  plot(x,y[,1],col="black",lwd=2,type="l")
  if (npred>1){
    for(j in 2:npred){
      lines(x,y[,j],type="l",col="grey",lwd=2)    
    }
  }
}



  
  
#Scoring curve 2 distributes the correct amount of money when distributing funds
#STILL TO UPDATE
scoring.curve.2 =function(Yreal,Ypred){
  npred=ncol(Ypred)
  nobs=nrow(Ypred)
  Y12=data
  
  y_min = min(Yreal)
  y_max = max(Yreal)
  x <- vector(length = length(nobs))
  y <- matrix(nrow = length(nobs), ncol = npred)
  #Improve the coding of this later
  sMatrix = matrix(s,nrow=nobs,length(s),byrow=TRUE)
  tempReal = matrix(Yreal,nrow=nobs,ncol=length(s))
  x = as.vector(colSums(tempReal < sMatrix)/nobs)   
  for(j in 1:npred){
    temp = matrix(Ypred[,j],nrow=nobs,ncol=length(s)) 
    y[,j] = as.vector(colSums(1*((temp < sMatrix)*(tempReal < sMatrix)))/colSums(1*(temp < sMatrix)))   
  }
  plot(x,y[,1],col="black",lwd=2,type="l")
  if (npred>1){
    for(j in 2:npred){
      lines(x,y[,j],type="l",col="grey",lwd=2)    
    }
  }
}






# code follows example here: http://www.r-bloggers.com/roc-curves-and-classification/

#note this assumes the same cutoff for Ypred and Yreal, not neccessarily what want
#This is equivalent however to the Grosh et al performance indicator - namely the y value of the ROC curve
#divided by the x value.
roc.curve=function(Ypred,Yreal,s,print=FALSE){
  Ppred=(Ypred<s)*1
  Preal=(Yreal<s)*1
  FP=sum((Ppred==1)*(Preal==0))/sum(Preal==0)
  TP=sum((Ppred==1)*(Preal==1))/sum(Preal==1)
  if(print==TRUE){
    print(table(Observed=Preal,Predicted=Ppred))
  }
  vect=c(FP,TP)
  names(vect)=c("FPR","TPR")
  return(vect)
}

ROC.curve=Vectorize(roc.curve, vectorize.args="s")


#Not yet working - want to plot a surface
roc.curve2=function(Ypred,Yreal,s){
  Ypred=as.vector(Ypred)
  Yreal=as.vector(Yreal)
  Preal=(Yreal<s)*1
  rule_cutoff_V = seq(min(Ypred),max(Ypred),by=.1)
  Ppred=outer(as.vector(Ypred),rule_cutoff_V,FUN="<")
  Preal=matrix(rep(Preal,length(rule_cutoff_V)),length(Preal),length(rule_cutoff_V))
  FP=(Ppred==1)*(Preal==0)
  TP=(Ppred==1)*(Preal==1)
  FPR=colSums(FP)/colSums(Preal==0)
  TPR=colSums(TP)/colSums(Preal==1)
  output=cbind(FPR,TPR)
  names(output)=c("FPR","TPR")
  return(output)
}
