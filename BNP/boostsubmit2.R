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

#d=5
start.time <- Sys.time()
boost.bnp1 = gbm(target ~ ., data = trainRoughFixed, distribution = "bernoulli", n.trees = 10000, interaction.depth=5, shrinkage=0.001, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
best.iter <- gbm.perf(boost.bnp1,method="OOB")
best.iter

yhat.bnp1 <- predict(boost.bnp1, newdata = testRoughFixed, n.trees=best.iter, type='response')
write.csv(data.frame(ID = testRoughFixed$ID, PredictedProb = yhat.bnp1), "boost_tree_benchmark1.csv", row.names = F)
#.47434

#d=6
start.time <- Sys.time()
boost.bnp2 = gbm(target ~ ., data = trainRoughFixed, distribution = "bernoulli", n.trees = 1000, interaction.depth=6, shrinkage=0.01, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
best.iter <- gbm.perf(boost.bnp2,method="OOB")
best.iter

yhat.bnp2 <- predict(boost.bnp2, newdata = testRoughFixed, n.trees=best.iter, type='response')
write.csv(data.frame(ID = testRoughFixed$ID, PredictedProb = yhat.bnp2), "boost_tree_benchmark2.csv", row.names = F)

#d=7
start.time <- Sys.time()
boost.bnp3 = gbm(target ~ ., data = trainRoughFixed, distribution = "bernoulli", n.trees = 1200, interaction.depth=7, shrinkage=0.001, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
best.iter <- gbm.perf(boost.bnp3,method="OOB")
best.iter

yhat.bnp3 <- predict(boost.bnp3, newdata = testRoughFixed, n.trees=best.iter, type='response')
write.csv(data.frame(ID = testRoughFixed$ID, PredictedProb = yhat.bnp3), "boost_tree_benchmark3.csv", row.names = F)

#d=8
start.time <- Sys.time()
boost.bnp4 = gbm(target ~ ., data = trainRoughFixed, distribution = "bernoulli", n.trees = 10000, interaction.depth=8, shrinkage=0.001, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
best.iter <- gbm.perf(boost.bnp4,method="OOB")
best.iter

yhat.bnp4 <- predict(boost.bnp4, newdata = testRoughFixed, n.trees=best.iter, type='response')
write.csv(data.frame(ID = testRoughFixed$ID, PredictedProb = yhat.bnp4), "boost_tree_benchmark4.csv", row.names = F)
