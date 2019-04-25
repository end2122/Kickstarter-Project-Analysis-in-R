setwd("~/Columbia MSBA/Spring 2019/E4650 - Business Analytics/Project - Kickstarter Analysis/raw_data")
df = read.csv("df_kickstarterFE.csv",header=TRUE,",")
df = df[-c(1)]
df = na.omit(df)
df = subset(df, select=-c(blurb,name))
df = na.omit(df)
head(df,2)
length(df)
library("glmnet")
library("leaps")
library("caret")

################### Model Setups #############################
x = as.matrix(df[,-9])
y = df$usd_pledged

ss = sample(1:3, size=nrow(df),replace=TRUE,prob=c(0.5,0.25,0.25))
df_train = df[ss==1,]
df_test = df[ss==2,]
df_validation = df[ss==3,]

train = ss==1
test = ss==2
validation = ss==3


################### Linear Model #############################



################### Logistic Model #############################
### Lasso ###
grid=10^(-3:3)
cv.lasso.log = cv.glmnet(x[train,],y[train], family='gaussian', alpha=1, lambda=grid, standardize=TRUE, type.measure='mse')
bestlam = cv.lasso.log$lambda.min

lasso.log.mod = glmnet(x[train,], y[train], family='gaussian', alpha=1, lambda=bestlam, standardize=TRUE)
coef(lasso.log.mod)

lasso.log.pred = predict(lasso.log.mod, x[validation,],na.rm=TRUE, type='response');
table(lasso.log.pred,y[validation])
lasso.log.error = sum(lasso.log.pred != y[validation])/length(lasso.log.pred)
lasso.log.error

################### KNN Model #############################




################### Clustering Model #############################
### Hierarchical ###


### K-Means ###




