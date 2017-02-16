#Random Forest on BNP data
library(randomForest)

set.seed(1)

trainData <- read.csv("train.csv")
testData <- read.csv("test.csv")

trainRoughFix <- na.roughfix(trainData)
testRoughFix <- na.roughfix(testData)
trainRoughFixed <- trainRoughFix[,-127]
trainRoughFixed <- trainRoughFixed[,-115]
trainRoughFixed <- trainRoughFixed[,-58]
trainRoughFixed <- trainRoughFixed[,-24]
trainRoughFixed <- trainRoughFixed[,-1]
testRoughFixed <- testRoughFix[,-126]
testRoughFixed <- testRoughFixed[,-114]
testRoughFixed <- testRoughFixed[,-57]
testRoughFixed <- testRoughFixed[,-23]
trainRoughFixed <- trainRoughFixed[!trainRoughFixed$target=='-',]
trainRoughFixed <- na.omit(trainRoughFixed)
trainTrain = sample(1:nrow(trainRoughFixed), nrow(trainRoughFixed)/2)
train.test = trainRoughFixed[-trainTrain,]

rf.bnp = randomForest(as.factor(target) ~ ., data = trainRoughFixed, subset = trainTrain, mtry = 11, ntree = 500)

#analyze results
yhat.rf = predict(rf.bnp,newdata=trainRoughFixed[-trainTrain,])
table(yhat.rf,train.test$target)
summary(yhat.rf)
importance(rf.bnp)
varImpPlot(rf.bnp)

subsettedTrain = trainRoughFixed[, c("target", "v50", "v112", "v52", "v10", "v12", "v14", "v114", "v34", "v40", "v21")]
#sqrt(10) = 3
rf.bnpSubsetted = randomForest(as.factor(target) ~ ., data = subsettedTrain, subset = trainTrain, mtry = 3, ntree = 500)
yhat.rfSubsetted = predict(rf.bnpSubsetted,newdata=subsettedTrain[-trainTrain,])
table(yhat.rfSubsetted,train.test$target)
summary(yhat.rfSubsetted)
importance(rf.bnpSubsetted)
varImpPlot(rf.bnpSubsetted)

yhat.test = predict(rf.bnp, newdata=testRoughFixed)
write.csv(data.frame(ID = test$ID, PredictedProb = yhat), "random_forest_benchmark.csv", row.names = F)