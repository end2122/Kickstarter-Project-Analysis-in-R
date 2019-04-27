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
mean((actual_lasso-pred_lasso)^2) #8.395986
#predict_success = ifelse(pred_subset>ks_LM_test$goal,1,0)
#mean(predict_success == (ks[test,])$state) #0.495

# net gain metrics
gain=(ks_LM_test$goal>ks_LM_test$usd_pledged) & (pred_lasso<ks_LM_test$usd_pledged)
gain_sum=sum(pred_lasso[gain])*1.064 
loss_1=(ks_LM_test$goal<ks_LM_test$usd_pledged) & (pred_lasso>ks_LM_test$usd_pledged)
loss_sum_1=sum(ks_LM_test$usd_pledged[loss_1])
loss_2=(ks_LM_test$goal<ks_LM_test$usd_pledged) & (pred_lasso<ks_LM_test$usd_pledged)
loss_sum_2=(sum(ks_LM_test$usd_pledged[loss_2]-pred_lasso[loss_2]*1.064))

gain_sum-loss_sum_1-loss_sum_2#-14800.64

gain=(ks_LM_test$goal>ks_LM_test$usd_pledged) 
sum(ks_LM_test$usd_pledged[gain])#85377.31