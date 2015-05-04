#####################################################################
## This file takes file intermediate_tanzania_for_R_1 and uses it to
## compare linear regression, LASSO, Ridge regressions and regresion trees.
## It compares the techniques for predicting consumption and poverty,
## using baseline covariates, both for the baseline year and future years
#####################################################################

#rm(list=ls())

setwd("C:/Users/Jack/Box Sync/Poverty Targeting/LSMS_ISA/Tanzania/")
data_in = "intermediate/data/intermediate_tanzania_for_R_1.dta"
var_table_tanzania = "variable_tables/variable_table_tanzania.xlsx"


####################################################################
### Setting up the data
####################################################################

### Import data

library(foreign)
Tanzania.panel <- read.dta(data_in)
names(Tanzania.panel)


### Creating variable lists

library(xlsx)
Tanzania.var_table <- read.xlsx(var_table_tanzania,sheetName="Var (Final)")

#Just using priority 1 variables for the time being
covariates.categorical = as.vector(Tanzania.var_table$var_name[Tanzania.var_table$type=="Categorical"
                                                               &Tanzania.var_table$priority==1&Tanzania.var_table$exists_2008_09==1])
covariates.cardinal = as.vector(Tanzania.var_table$var_name[Tanzania.var_table$type=="Cardinal"
                                                            &Tanzania.var_table$priority==1&Tanzania.var_table$exists_2008_09==1])
covariates.yesno = as.vector(Tanzania.var_table$var_name[Tanzania.var_table$type=="Yes/No"
                                                         &Tanzania.var_table$priority==1&Tanzania.var_table$exists_2008_09==1])
covariates.all = c(covariates.categorical,covariates.cardinal,covariates.yesno)


### Ensuring R has casted the variables as it should have

str(Tanzania.panel[c(covariates.categorical)])
Tanzania.panel[,c(covariates.categorical)] <- lapply(Tanzania.panel[,c(covariates.categorical)], as.factor)
str(Tanzania.panel[c(covariates.categorical)])
#It worked!
Tanzania.panel[,c(covariates.cardinal)] <- lapply(Tanzania.panel[,c(covariates.cardinal)], as.numeric)
Tanzania.panel[,c(covariates.yesno)] <- lapply(Tanzania.panel[,c(covariates.yesno)], as.logical)
#Note, may wish to recode some of the categoricals as ordered and the yes/no as categorical.


### Creating new dependent variables

Tanzania.panel$lconsPC = log(Tanzania.panel$expmR %/% Tanzania.panel$hhsize) 
Tanzania.panel$lconsPAE = log(Tanzania.panel$expmR %/% Tanzania.panel$adulteq)


#08

#Temporary fix for missing variables (algorithms don't run with them)
Tanzania.08.PC <- Tanzania.panel[Tanzania.panel$year==2008&complete.cases(Tanzania.panel),c("lconsPC",covariates.all)]

#Normal version
yx.08 <- data.frame(y=Tanzania.08.PC$lconsPC, x=Tanzania.08.PC[,covariates.all])
#Model Matrix version
yx.MM.08 <- data.frame(y=Tanzania.08.PC$lconsPC, x=model.matrix(lconsPC~.,Tanzania.08.PC))

set.seed(1)      
train=sample(1:nrow(yx.08), nrow(yx.08)/2)
test=(-train)
#standardizing x-variables - only needed for MM version, which goes to Lasso and Ridge
yx.MM.08<-standardize.x(yx.MM.08)

yx.train.08=yx.08[train,]
yx.test.08=yx.08[test,]

yx.MM.train.08=yx.MM.08[train,]
yx.MM.test.08=yx.MM.08[test,]

ridge.08.08 <-ridge.predict.kfold(yx.MM.08,10,1)
lasso.08.08 <-lasso.predict.kfold(yx.MM.08,10,1)

temp <- reg.predict(yx.train.08,yx.test.08,c("x.wall_mat","x.toilet","x.children"))
reg.08.08 <- reg.predict.kfold(yx.08,10,1,c("x.wall_mat","x.toilet","x.children"))

temp <-tree.predict(yx.train.08,yx.test.08)
tree.08.08 <-tree.predict.kfold(yx.08,10,1)

temp <-regfit.predict(yx.train.08,yx.test.08,20)
regfit.08.08 <- regfit.predict.kfold(yx.MM.08,10,1,20)

scoring.curve.1.kfold(ridge.08.08)

par( mfrow = c( 2, 2 ) )
ROC.curve.1.kfold(ridge.08.08,12.5)
ROC.curve.1.kfold(lasso.08.08,12.5)
ROC.curve.1.kfold(regfit.08.08,12.5)
ROC.curve.1.kfold(tree.08.08,12.5)