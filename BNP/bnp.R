library(tree)
library(ISLR)
library(randomForest)
library(gbm)
library(e1071)
library(ROCR)
library(leaps)
library(usdm)

#confusion function
confusion <- function(a, b){
  tbl <- table(a, b)
  mis <- 1 - sum(diag(tbl))/sum(tbl)
  list(table = tbl, misclass.prob = mis)
}

#set seed for replication
set.seed(1)

trainData <- read.csv("train.csv")
testData <- read.csv("test.csv")

trainRoughFix <- na.roughfix(trainData)
testRoughFix <- na.roughfix(testData)
trainRoughFixed <- trainRoughFix[,-127]
trainRoughFixed <- trainRoughFixed[,-115]
trainRoughFixed <- trainRoughFixed[,-58]
trainRoughFixed <- trainRoughFixed[,-24]
trainRoughFixed <- trainRoughFixed[!trainRoughFixed$target=='-',]
trainRoughFixed <- na.omit(trainRoughFixed)
trainTrain = sample(1:nrow(trainRoughFixed), nrow(trainRoughFixed)/2)
train.test = trainRoughFixed[-trainTrain,]
smallTrain = sample(1:nrow(trainRoughFixed), nrow(trainRoughFixed)/100)


table(as.factor(trainRoughFixed$target))
#If we predicted "yes" or 1 every time, then we would get it right 87021/114321 = 76.12% of the time

#Explore
plot(trainRoughFixed$target,trainRoughFixed$v50)
plot(trainRoughFixed$target,trainRoughFixed$v2)


'''
#Fitting Classification Tree
str(trainRoughFixed)
str(trainRoughFixed[,90:129])
tree.bnp = tree(as.factor(target) ~ . - ID, trainRoughFixed)
summary(tree.bnp)
plot(tree.bnp)
text(tree.bnp,pretty=0)
#this predicted 1 every time
'''

#Best subset selection
file <- system.file("external/spain.grd", package="usdm")

r <- brick(file) # reading a RasterBrick object including 10 raster layers in Spain

r 
vif(r)
vif(subsettedTrain[smallTrain,])
regfit.full = regsubsets(as.factor(target)~ . - )

#Random forests
#Bagged Tree with sqrt(p) predictors considered at each split, 100 trees
rf.bnp = randomForest(as.factor(target) ~ . - ID, data = trainRoughFixed, subset = trainTrain, mtry = 11, ntree = 500)

#analyze results
yhat.rf = predict(rf.bnp,newdata=trainRoughFixed[-trainTrain,])
table(yhat.rf,train.test$target)
summary(yhat.rf)
#42521 correct yes predictions
#11387 incorrect yes predictions (false positives)
#42521/53908 = 78.88% correct yes predictions

#2143 correct no predictions
#1110 incorrect no predictions (false negatives)
#2143/3253 = 65.88% correct no predictions

#44664/57161 = 78.14% correct overall
#the overall percent correct went up a few percent from just guessing one every time

importance(rf.bnp)
varImpPlot(rf.bnp)
#Gini index lists v50, v112, v52, v10, v12 as top 5 most important, others close by then tails off to ~80



#Boosting
library(rpart)


boost.bnpd1s1 = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 300, interaction.depth=1, verbose = TRUE)
boost.bnpd2s1 = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 300, interaction.depth=2, verbose = TRUE)
boost.bnpd3s1 = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 300, interaction.depth=3, verbose = TRUE)
boost.bnpd4s1 = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 300, interaction.depth=4, verbose = TRUE)
boost.bnpd5s1 = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 300, interaction.depth=5, verbose = TRUE)
boost.bnpd6s1 = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 300, interaction.depth=6, verbose = TRUE)
boost.bnpd7s1 = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 300, interaction.depth=7, verbose = TRUE)

start.time <- Sys.time()
boost.bnp = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 3000, interaction.depth=4, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
#3 min
#gbm.perf(boost.bnp, method="test")
#range(boost.bnp)

#OOB underestimates the optimal number of iterations
best.iter <- gbm.perf(boost.bnp,method="OOB")
best.iter
best.iter <- gbm.perf(boost.bnp,method="test")
best.iter
best.iter <- gbm.perf(boost.bnp,method="cv")
best.iter

summary(boost.bnp)

yhat.logods = predict(boost.bnp, newdata = trainRoughFixed[-trainTrain,], n.trees=best.iter)
yhat.trueProb1 = 1/(1+exp(-yhat.logods))
yhat.prob = predict(boost.bnp, newdata = trainRoughFixed[-trainTrain,], n.trees=best.iter, type='response')
yhat.logloss = trainRoughFixed[-trainTrain,]$target*log(yhat.prob) + (1-trainRoughFixed[-trainTrain,]$target)*log(1-yhat.prob)
yhat.prob[1:10]
confusion(yhat.prob > .8, train.test$target < 1)
summary(boost.bnp)
yhat.prob[1:100]



#SVMs
#svmfit=svm(as.factor(target) ~ . - ID, data = trainRoughFixed[trainTrain,], kernel ="radial",gamma=1,cost=1)
start.time <- Sys.time()
svmfit=svm(as.factor(target) ~ . - ID, data = trainRoughFixed[smallTrain,], kernel ="radial",gamma=1,cost=1)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
#6 min at 1/10 all data, 3 seconds at 1/100
plot(svmfit, trainRoughFixed[smallTrain,])
summary(svmfit)

start.time <- Sys.time()
tune.out=tune(svm,as.factor(target) ~ . - ID, data = trainRoughFixed[smallTrain,], kernel ="radial",ranges=list(cost=c(0.001,0.01,0.1,1,10,100,1000),gamma=c(0.01,0.1,0.5,1,2,3,4,5,10,20)))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

smallTest <- trainRoughFixed[-smallTrain,]
smallTestSample <- sample(1:nrow(smallTest), nrow(smallTest)/100)
plot(svmfit, trainRoughFixed[smallTrain,])
summary(svmfit)

#SVMs with less variables
#subsetted variables using mean decrease gini
subsettedTrain = trainRoughFixed[, c("target", "v50", "v112", "v52", "v10", "v12", "v14", "v114", "v34", "v40", "v21")]

start.time <- Sys.time()
svmfit=svm(as.factor(target) ~ ., data = subsettedTrain[smallTrain,], kernel ="radial",gamma=1,cost=100)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
#6 min at 1/10 all data, 3 seconds at 1/100
plot(svmfit, trainRoughFixed[smallTrain,])
summary(svmfit)

start.time <- Sys.time()
tune.out=tune(svm,as.factor(target) ~ . - ID, data = subsettedTrain[smallTrain,], kernel ="radial",ranges=list(cost=c(0.001,0.01,0.1,1,10,100,1000),gamma=c(0.01,0.1,0.5,1,2,3,4,5,10,20)))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken