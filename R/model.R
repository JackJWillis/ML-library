#############
### This file contains ML prediction functions for different algorithms,
### designed to be harmonized
#############

library(glmnet)  
library(magrittr)


# Data processing ---------------------------

#' Standardize a dataset so that predictor variables have unit variance.
#' 
#' @param yx A dataframe. The first column should be the variable to be predicted.
standardize_x  <-  function(yx) {
  x <- yx[,1]
  numeric_features <- sapply(x, is.numeric)
  x[, numeric_features] <- scale(x[, numeric_features], center=True, scale=True)

  # Standardizing can cause some variables to become NA (if they are constant)
  # Drop those variables
  x <- x[, colSums(is.na(x)) < nrow(x)]
  yx[, -1] <- x
  yx
}

# Linear models ---------------------------

ridge_predict <- function(x_train, y_train, x_test) {
  glmnet::cv.glmnet(x_train, y_train, alpha=0) %>%
    glmnet::predict_linear(x_test)
}


lasso_predict <- function(x_train, y_train, x_test) {
  glmnet::cv.glmnet(x_train, y_train, alpha=1) %>%
    glmnet::predict_linear(x_test)
}


least_squares_predict <- function(x_train, y_train, x_test) {
  glmnet::cv.glmnet(x_train, y_train, lambda=0) %>%
    glmnet::predict_linear(x_test)
}



# K fold validation ---------------------------

kfold <- function(k, predfun, x, y, seed=0) {
  y <- y[[1]]
  x <- data.matrix(x)
  set.seed(seed)
  folds <- sample(1:k, nrow(x), replace=TRUE) #TODO load balance
  residuals <- sapply(1:k, function (k) {
    y[folds == k] - predfun(x[folds != k], y[folds != k], x[folds == k])
  })
  mean(residuals ** 2)
}


# ridge.predict = function(x_train,y_train,x_test,y_test){
#   grid<-10^seq(10,-2,length=100)
#   Ridge.mod<-glmnet(x_train,y_train,alpha=0,lambda=grid,thresh=1e-12)
#   cv.out<-cv.glmnet(x_train,y_train,alpha=0)
#   bestlam<-cv.out$lambda.min
#   pred<-predict(Ridge.mod,s=bestlam,newx=x_test)
#   coef<-predict(Ridge.mod,s=bestlam,type="coefficients")
#   MSE<-mean((pred-y_test)^2)
#   out<-data.frame(y_pred=pred,y_real=y_test)
#   #This won't assign name y_pred, don't know why. Hence the following
#   names(out)[1]<-"y_pred"
#   ridge<-list(out=out,coef=coef,MSE=MSE,bestlam=bestlam)
#   return(ridge)
# }
# 
# #Function which will run cross validated output on whole data
# ridge.predict.kfold = function(yx,k,s){
#   set.seed(s) 
#   y <- yx[[1]]
#   x <- data.matrix(yx[,-1])
#   folds<-sample(1:k,nrow(x),replace=TRUE)
#   ridge.folds <- list()
#   for(j in 1:k){
#     ridge.folds[[j]] <- ridge.predict(x[folds!=j,],y[folds!=j],x[folds==j,],y[folds==j])
#     names(ridge.folds)[j] <- paste("fold_",j,sep="")
#     #Following is to add indexing information for the y's within the folds
#     ridge.folds[[j]]$indices <- which(folds==j)
#   }
#   return(ridge.folds)
# }
# 
# 
# ### Lasso
# 
# #Assumes independent variables already in model matrix form
# 
# lasso.predict = function(x_train,y_train,x_test,y_test){
#   grid<-10^seq(10,-2,length=100)
#   Lasso.mod<-glmnet(x_train,y_train,alpha=1,lambda=grid)
#   cv.out<-cv.glmnet(x_train,y_train,alpha=1)
#   bestlam<-cv.out$lambda.min
#   pred<-predict(Lasso.mod,s=bestlam,newx=x_test)
#   coef<-predict(Lasso.mod,s=bestlam,type="coefficients")
#   MSE<-mean((pred-y_test)^2)
#   out<-data.frame(y_pred=pred,y_real=y_test)
#   #This won't assign name y_pred, don't know why. Hence the following
#   names(out)[1]<-"y_pred"  
#   lasso<-list("out"=out,"coef"=coef,"MSE"=MSE,"bestlam"=bestlam)
#   return(lasso)
# }
# 
# #Function which will run cross validated output on whole data
# lasso.predict.kfold = function(yx,k,s){
#   set.seed(s) 
#   y <- yx[[1]]
#   x <- data.matrix(yx[,-1])
#   folds=sample(1:k,nrow(x),replace=TRUE)
#   lasso.folds <- list()
#   for(j in 1:k){
#     lasso.folds[[j]] <- lasso.predict(x[folds!=j,],y[folds!=j],x[folds==j,],y[folds==j])
#     names(lasso.folds)[j] <- paste("fold_",j,sep="")
#   }
#   return(lasso.folds)
# }
# 
# ### Straight regression
# 
# reg.predict = function(yx_train,yx_test,varlist){
#   #varlist is a vector of names of variables to be included in regression
#   yx_train_red <- cbind(yx_train[,1],yx_train[,varlist])
#   yx_test_red <- cbind(yx_test[,1],yx_test[,varlist])
#   names(yx_train_red)[1]<-"Y"
#   fit<-lm(Y~.,data=yx_train_red)
#   pred<-predict(fit,newdata=yx_test_red[,-1])
#   coef<-fit$coefficients
#   MSE=mean((pred-yx_test_red[[1]])^2)
#   out=data.frame(y_pred=pred,y_real=yx_test_red[[1]])
#   reg=list(out=out,coef=coef,MSE=MSE)
#   return(reg)
# }
# 
# #Function which will run cross validated output on whole data
# reg.predict.kfold = function(yx,k,s,varlist){
#   set.seed(s) 
#   folds=sample(1:k,nrow(yx),replace=TRUE)
#   reg.folds <- list()
#   for(j in 1:k){
#     reg.folds[[j]] <- reg.predict(yx[folds!=j,],yx[folds==j,],varlist)
#     names(reg.folds)[j] <- paste("fold_",j,sep="")
#   }
#   return(reg.folds)
# }
# 
# 
# ### Stepwise regression
# 
# library(leaps)
# 
# 
# predict.regsubsets.2=function(object,newdata,id,...){ 
#   mat=data.matrix(newdata) 
#   coefi=coef(object,id=id)
#   xvars=names(coefi)
#   mat[,xvars]%*%coefi
# }
# 
# #First creating a predict function since regsubsets() doesn't have one
# predict.regsubsets=function(object,newdata,id,...){ 
#   form=as.formula(as.character(object$call[[2]])) ## extract formula
#   form <- update(form, Y ~ .)
#   mat=model.matrix(form,newdata)
#   coefi=coef(object,id=id)
#   xvars=names(coefi)
#   mat[,xvars]%*%coefi
# }
# 
# #Now main function, which follows a similar form to Lasso and ridge
# regfit.predict = function(yx_train,yx_test,nmax){
#   set.seed(1)
#   k=10
#   names(yx_train)[1]<-"Y"
#   names(yx_test)[1]<-"Y"
#   folds=sample(1:k,nrow(yx_train),replace=TRUE)
#   cv.errors=matrix(NA,k,nmax, dimnames=list(NULL, paste(1:nmax)))
#   for(j in 1:k){
#     best.fit=regsubsets(Y~.,data=yx_train[folds!=j,],nvmax=nmax,method="seqrep")
#     for(i in 1:nmax){
#       #Here is where the problem is:
#       pred=predict(best.fit,yx_train[folds==j,],id=i)
#       cv.errors[j,i]=mean((yx_train$Y[folds==j]-pred)^2)
#     }
#   }
#   mean.cv.errors=apply(cv.errors,2,mean)
#   bestn = which.min(mean.cv.errors)
#   reg.best=regsubsets(Y~.,data=yx_train,nvmax=nmax,method="seqrep")
#   pred=predict(reg.best,yx_test,id=bestn)  
#   out=data.frame(y_pred=pred,y_real=yx_test[[1]])
#   coef=coef(reg.best,bestn)
#   MSE=mean((pred-yx_test[[1]])^2)
#   regfit=list(out=out,coef=coef,MSE=MSE,bestn=bestn)
#   return(regfit)
# }
# 
# #Function which will run cross validated output on whole data
# #NOT ORKIGN!!
# regfit.predict.kfold = function(yx,k,s,nmax){
#   set.seed(s) 
#   folds=sample(1:k,nrow(yx),replace=TRUE)
#   reg.folds <- list()
#   for(j in 1:k){
#     temp.yx.train.08 <- yx[folds!=j,]
#     temp.yx.test.08 <- yx[folds==j,]
#     regfit.folds[[j]] <- regfit.predict(temp.yx.train.08,temp.yx.test.08,nmax)
#     names(regfit.folds)[j] <- paste("fold_",j,sep="")
#   }
#   return(regfit.folds)
# }
# 
# 
# ###Regression Tree
# 
# library(tree)
# 
# tree.predict = function(yx_train,yx_test){
#   #Have to generate a global version otherise cv.trees doesn't ork. See here:
#   #http://stackoverflow.com/questions/28148533/is-data-framedata-object-not-found-in-function-context
#   yx_train_global <<- yx_train
#   names(yx_train_global)[1]<<-"Y"
#   tree.first <- tree(Y~.,yx_train_global)
#   cv.trees <- cv.tree(tree.first)
#   #Chooses tree size with minimal deviance
#   bestsize <- cv.trees$size[which.min(cv.trees$dev)]
#   tree.final <- prune.tree(tree.first, best = bestsize)
#   pred=predict(tree.final,newdata=yx_test[,-1])
#   pred.tree=predict(tree.final,newdata=yx_test[,-1],type="tree")
#   MSE=mean((pred-yx_test[[1]])^2)
#   out=data.frame(y_pred=pred,y_real=yx_test[[1]])
#   tree=list(out=out,tree=pred.tree,MSE=MSE,bestsize=bestsize)
#   yx_train_global <<- NA
#   return(tree)  
# }
# 
# 
# #Function which will run cross validated output on whole data
# tree.predict.kfold = function(yx,k,s){
#   set.seed(s) 
#   folds=sample(1:k,nrow(yx),replace=TRUE)
#   tree.folds <- list()
#   for(j in 1:k){
#     tree.folds[[j]] <- tree.predict(yx[folds!=j,],yx[folds==j,])
#     names(tree.folds)[j] <- paste("fold_",j,sep="")
#   }
#   return(tree.folds)
# }
# 
# 
# ###Random forests
# 
# library(randomForest)
# 
#   # TO FILL
# 
# 
# ###Conditional inference trees
# 
# library(party)
# 
# CIT.predict = function(yx_train,yx_test){
#   names(yx_train)[1]<-"Y"
#   fit <- ctree(Y~.,data=yx_train)
#   
#   # TO FILL  
# }
# 
