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
testRoughFixed <- testRoughFixed[,-1]

testRoughFixed$target <- 0
allRoughFixed <- rbind(trainRoughFixed, testRoughFixed)

start.time <- Sys.time()
rf.bnp = randomForest(as.factor(target) ~ ., data = allRoughFixed[1:114321,], mtry = 11, ntree = 500)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

yhat.rf = predict(rf.bnp,newdata=allRoughFixed[114322:228714,],type="prob")
table(yhat.rf,allRoughFixed[-trainTrain & 1:114321,]$target)
yhat.rf

#analyze results
yhat.rf = predict(rf.bnp,newdata=allRoughFixed[101:200,])
yhat.rf
table(yhat.rf,allRoughFixed[101:200,]$target)
summary(yhat.rf)
importance(rf.bnp)
varImpPlot(rf.bnp)


yhat.test = predict(rf.bnp, newdata=testRoughFixed)
write.csv(data.frame(ID = testData$ID, PredictedProb = yhat.rf[,2]), "random_forest_attempt_one.csv", row.names = F)
