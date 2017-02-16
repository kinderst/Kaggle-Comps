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
trainTrain = sample(1:nrow(trainRoughFixed), nrow(trainRoughFixed)/100)
train.test = trainRoughFixed[-trainTrain,]
train.test = train.test[1:1000,]
testRoughFixed <- testRoughFixed[1:100,]
trainRoughFixed <- trainRoughFixed[1:100,]
testRoughFixed <- testRoughFixed[,-1]
testRoughFixed$target <- 0
testRoughFixed$target
allRoughFixed <- rbind(trainRoughFixed, testRoughFixed)

rf.bnp = randomForest(as.factor(target) ~ ., data = allRoughFixed[1:100,], mtry = 11, ntree = 500)




#analyze results
yhat.rf = predict(rf.bnp,newdata=allRoughFixed[101:200,])
yhat.rf
table(yhat.rf,allRoughFixed[101:200,]$target)
summary(yhat.rf)
importance(rf.bnp)
varImpPlot(rf.bnp)


yhat.test = predict(rf.bnp, newdata=testRoughFixed)
write.csv(data.frame(ID = test$ID, PredictedProb = yhat), "random_forest_benchmark.csv", row.names = F)