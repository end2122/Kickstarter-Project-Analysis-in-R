data = read.csv("df_kickstarterFE.csv")

#remove nan
data = na.omit(data)

#remove char and state
rmvdData <- data[-c(1,6,8,13)]

#remove duplicated rows
library(dplyr)
deduped.data = unique(rmvdData [,1:ncol(rmvdData)])

#log values
deduped.data $usd_pledged = log1p(deduped.data $usd_pledged)
deduped.data $goal = log1p(deduped.data $goal)
deduped.data $backers_count=scale(deduped.data $backers_count)
deduped.data $Launch_deadline_days=scale(deduped.data $Launch_deadline_days)

newData=deduped.data 

# Divide training and test sets
set.seed(1)
train = sample(1:nrow(newData),0.75*nrow(newData)) 
test = -train
training=newData[train,]
validation=newData[test,]

summary(training)
y=newData$usd_pledged
y.validation = y[test,]
y.state = data$state[test,]

##################### Partial Least Squares (PLS) Using 10-fold-CV ########################################
pls.fit = plsr(usd_pledged~., data= training, scale=T, validation = "CV", segments=10)
validationplot(pls.fit,val.type="MSEP")
pls.pred = predict(pls.fit,validation,ncomp=3)
mean((pls.pred - y.validation)^2) # 8.395504
# calculate accuracy
#validation$pls = ifelse(pls.pred>validation$goal,1,0)
#mean(validation$pls!= y.state) # 0.4212695

############################calculate profits##########################
gain=(validation$goal>validation$usd_pledged) & (pls.pred<validation$usd_pledged)
gain_sum=sum(pls.pred[gain])*1.064 
loss_1=(validation$goal<validation$usd_pledged) & (pls.pred>validation$usd_pledged)
loss_sum_1=sum(validation$usd_pledged[loss_1])
loss_2=(validation$goal<validation$usd_pledged) & (pls.pred<validation$usd_pledged)
loss_sum_2=(sum(validation$usd_pledged[loss_2]-pls.pred[loss_2]*1.064))
gain_sum-loss_sum_1-loss_sum_2#-14773.43

############################################ Decision Tree ###############################################
# Load rpart and rpart.plot
install.packages("tree")
library(tree)
library(MASS)
library(ISLR)
library(rpart)
tree.pldg=tree(usd_pledged~.,training)
cv.pldg=cv.tree(tree.pldg)
plot(cv.pldg$size,cv.pldg$dev,type='b')
prune.pldg=prune.tree(tree.pldg,best=cv.pldg$size[which.min(cv.pldg$dev)])

# Prediction
yhat=predict(prune.pldg,newdata=validation)
mean((yhat-y.validation)^2) #1.055145

# calculate accuracy
#validation$tree = ifelse(yhat>validation$goal,1,0)
#mean(validation$tree != y.state) #0.4681237

############################calculate profits##########################
gain_tree=(validation$goal>validation$usd_pledged) & (yhat<validation$usd_pledged)
gain_sum_tree=sum(yhat[gain_tree])*1.064 
loss_1_tree=(validation$goal<validation$usd_pledged) & (yhat>validation$usd_pledged)
loss_sum_1_tree=sum(validation$usd_pledged[loss_1_tree])
loss_2_tree=(validation$goal<validation$usd_pledged) & (yhat<validation$usd_pledged)
loss_sum_2_tree=(sum(validation$usd_pledged[loss_2_tree]-yhat[loss_2_tree]*1.064))
gain_sum_tree-loss_sum_1_tree-loss_sum_2_tree #-48004.3



########################################## Random Forrest #################################################
install.packages("randomForest")
library(randomForest)
rf.pldg=randomForest(usd_pledged~.,data=training,mtry=2,ntree = 4, importance=TRUE)
# Prediction
yhat.rf = predict(rf.pldg,newdata=validation)
mean((yhat.rf-y.validation)^2) #0.8816235
# Calculate accuracy
#validation$rf = ifelse(yhat.rf>validation$goal,1,0)
#mean(validation$rf != y.state) #0.495147
# Most important predictor
importance(rf.pldg)
varImpPlot(rf.pldg)

############################calculate profits##########################
gain_forest=(validation$goal>validation$usd_pledged) & (yhat.rf<validation$usd_pledged)
gain_sum_forest=sum(yhat.rf[gain_forest])*1.064 
loss_1_forest=(validation$goal<validation$usd_pledged) & (yhat.rf>validation$usd_pledged)
loss_sum_1_forest=sum(validation$usd_pledged[loss_1_forest])
loss_2_forest=(validation$goal<validation$usd_pledged) & (yhat.rf<validation$usd_pledged)
loss_sum_2_forest=(sum(validation$usd_pledged[loss_2_forest]-yhat.rf[loss_2_forest]*1.064))
gain_sum_forest-loss_sum_1_forest-loss_sum_2_forest #-35326.32

