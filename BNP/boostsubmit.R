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

start.time <- Sys.time()
boost.bnp = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 1800, interaction.depth=6, shrinkage=0.05, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken