setwd("/Users/qifuyin/desktop")
ks = read.csv("df_kickstarterFE.csv")

#Linear Regression
set.seed(1)
library(leaps)
library(glmnet)
ks_LM = ks[-c(1,6,8,13)]
ks_LM$usd_pledged = log1p(ks_LM$usd_pledged)
ks_LM$goal = log1p(ks_LM$goal)
ks_LM$backers_count=scale(ks_LM$backers_count)
ks_LM$Launch_deadline_days=scale(ks_LM$Launch_deadline_days)
ks_LM=ks_LM[!duplicated(ks_LM), ]
train = sample(1:nrow(ks_LM),0.75*nrow(ks_LM)) 
test = -train
ks_LM_train=ks_LM[train,]
ks_LM_test=ks_LM[test,]
p = 8
k=10

#subset selection
folds=sample(1:k,nrow(ks_LM_train),replace=TRUE)
predict.regsubsets=function(regfit.fwd,newdata,t){
  form=as.formula(regfit.fwd$call[[2]])
  mat=model.matrix(form,newdata) 
  coefi=coef(regfit.fwd,id=t) 
  xvars=names(coefi)
  pred = mat[,xvars]%*%coefi 
  return(pred)
}
cv.errors=array(NA,dim=c(k,p)) 
for(j in 1:k){
  #find the best models for training set j
  regfit.fwd=regsubsets(usd_pledged ~., data=ks_LM_train[folds!=j,],nvmax=p, method="forward")
  for(t in 1:p){
    #predict use 1 to p features
    pred = predict.regsubsets(regfit.fwd,ks_LM_train[folds==j,],t)
    actual = ks_LM_train$usd_pledged[folds==j]
    cv.errors[j,t] = mean((actual-pred)^2)
  }
}
mean.cv.errors=apply(cv.errors,2,mean)
best.model = which.min(mean.cv.errors)#find the best model with features p
regfit.fwd=regsubsets(usd_pledged ~., data=ks_LM_train,nvmax=best.model, method="forward")
pred_subset=predict.regsubsets(regfit.fwd, ks_LM_test, best.model)
actual_subset = ks_LM_test$usd_pledged
mean((actual_subset - pred_subset)^2) #8.395992
best.model
coef(regfit.fwd,best.model)

#Lasso
x=model.matrix(usd_pledged~.,ks_LM )
y=ks_LM $usd_pledged
grid= c(0,0.001,0.01,0.1,1,10,100,1000)
cv.out = cv.glmnet(x[train,], y[train], alpha=1, lambda=grid, nfolds=k) 
bestlam = cv.out$lambda.min
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=bestlam)
bestlam
coef(lasso.mod)
pred_lasso = predict(lasso.mod, x[test,])
actual_lasso = y[test]
mean((actual_lasso-pred_lasso)^2) #8.395787
#predict_success = ifelse(pred_subset>ks_LM_test$goal,1,0)
#mean(predict_success == (ks[test,])$state) #0.495

# net gain metrics
gain=(ks_LM_test$goal>ks_LM_test$usd_pledged) & (pred_lasso<ks_LM_test$usd_pledged)
gain_sum=sum(pred_lasso[gain]) #sum(pred_subset[gain])
loss=(ks_LM_test$goal<ks_LM_test$usd_pledged) & (pred_lasso>ks_LM_test$usd_pledged)
loss_sum=sum(ks_LM_test$usd_pledged[loss])
gain_sum-loss_sum #14997.66 for pred_lasso

gain=(ks_LM_test$goal>ks_LM_test$usd_pledged) 
sum(ks_LM_test$usd_pledged[gain])#85377.31