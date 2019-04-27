library('FNN')
setwd("/Users/qifuyin/desktop")
ks = read.csv("df_kickstarterFE.csv")
ks_KNN = ks[-c(1,6,8,13)]
ks_KNN$usd_pledged = log1p(ks_KNN$usd_pledged)
ks_KNN$goal = log1p(ks_KNN$goal)
ks_KNN$backers_count=scale(ks_KNN$backers_count)
ks_KNN$Launch_deadline_days=scale(ks_KNN$Launch_deadline_days)
ks_KNN=ks_KNN[!duplicated(ks_KNN), ]
#dim(ks_KNN)

#KNN
set.seed(1)
train_ind = sample(1:nrow(ks_KNN), 0.5*nrow(ks_KNN))
train=ks_KNN[train_ind,1:ncol(ks_KNN)]
val_test=ks_KNN[-train_ind,1:ncol(ks_KNN)]
val_test_ind=sample(nrow(val_test),0.5*nrow(val_test))
val=val_test[val_test_ind,1:ncol(val_test)]
test=val_test[-val_test_ind,1:ncol(val_test)]

k.vec = 1:20
err.mat.knn = array(NA,dim=c(length(k.vec))) 
for(i in k.vec){
  knn.pred= knn.reg(train=train[,-8],test=val[,-8],y=train[,8],k=i)
  err.mat.knn[i]=mean((knn.pred$pred-val[,8])^2)
}
bestK = which.min(err.mat.knn) #mse=7.368647, bestK=14

train_val=rbind(train,val)
knn.pred= knn.reg(train=train_val[,-8],test=test[,-8],y=train_val[,8],k=bestK)
mean((knn.pred$pred-test[,8])^2)

gain=(test$goal>test$usd_pledged) & (knn.pred$pred<test$usd_pledged)
gain_sum=sum(knn.pred$pred[gain])*1.064
loss_1=(test$goal<test$usd_pledged) & (knn.pred$pred>test$usd_pledged)
loss_sum_1=sum(test$usd_pledged[loss_1])
loss_2=(test$goal<test$usd_pledged) & (knn.pred$pred<test$usd_pledged)
loss_sum_2=(sum(test$usd_pledged[loss_2]-knn.pred$pred[loss_2]*1.064))

gain_sum-loss_sum_1-loss_sum_2 #-14845.45
best_gain=test$goal>test$usd_pledged
sum(test$usd_pledged[best_gain])#85000.98


