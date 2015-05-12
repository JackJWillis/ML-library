#############
### This file contains graphing functions for plotting the output from the ML_functions
#############

plot_scatter <- function(kfold_results) {
  ggplot2::ggplot(kfold_results, ggplot2::aes(x=true, y=predicted)) +
    ggplot2::geom_point(alpha=0.5)
}

plot_density<- function(kfold_results) {
  melted <- reshape2::melt(kfold_results, id=c("fold"))
  ggplot2::ggplot(melted, ggplot2::aes(x=value, fill=variable)) +
    ggplot2::geom_density(alpha=0.5)
}

#' Produce an ROC curve which plots a given method's sensitivity/specificity with respect
#' a given poverty threshold.
plot_roc <- function(kfold_results, threshold) {
  response <- kfold_results$true < threshold
  roc <- pROC::roc(response, kfold_results$predicted)
  plot(roc)
}


get_cumulative <- function(kfold_results, threshold) {
  
}


#' If we target N people, what fraction of the true poor would receive funds?
plot_accuracy <- function(kfold_results, threshold, point_count=20) {
  N <- nrow(kfold_results)
  plot_points <- seq(1, N, length=point_count)
  
  ranked <- kfold_results[order(kfold_results$predicted), ]
  ranked$response <- ranked$true < threshold
  ranked$coverage <- cumsum(ranked$response) / sum(ranked$response)
  
  cut <- ranked[plot_points, ]
  cut$percent_population_included <- plot_points / N
  ggplot2::ggplot(cut, ggplot2::aes(x=percent_population_included, y=coverage)) +
    ggplot2::geom_step()
}


#' With a fixed amount of money, if we target N people, what fraction would go to the true poor?
plot_accuracy_dollars <- function(kfold_results, threshold, point_count=20) {
  N <- nrow(kfold_results)
  plot_points <- seq(1, N, length=point_count)
  
  ranked <- kfold_results[order(kfold_results$predicted), ]
  ranked$response <- ranked$true < threshold
  ranked$to_true_poor <- cumsum(ranked$response) / seq(1, nrow(ranked))
  
  cut <- ranked[plot_points, ]
  cut$percent_population_included <- plot_points / N
  ggplot2::ggplot(cut, ggplot2::aes(x=percent_population_included, y=to_true_poor)) +
    ggplot2::geom_step()
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
