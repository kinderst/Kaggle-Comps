#.47313 log loss

library(randomForest)
library(gbm)
library(tree)

set.seed(2016)

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

start.time <- Sys.time()
boost.bnp = gbm(target ~ ., data = trainRoughFixed, distribution = "bernoulli", n.trees = 1000, interaction.depth=6, shrinkage=0.01, cv.folds = 3, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
best.iter <- gbm.perf(boost.bnp,method="OOB")
best.iter

yhat.bnp <- predict(boost.bnp, newdata = testRoughFixed, n.trees=best.iter, type='response')
write.csv(data.frame(ID = testRoughFixed$ID, PredictedProb = yhat.bnp), "boost_tree_benchmark.csv", row.names = F)
