library(randomForest)
library(gbm)
library(tree)

MultiLogLoss <- function(act, pred)
{
  eps = 1e-15;
  nr <- nrow(pred)
  pred = matrix(sapply( pred, function(x) max(eps,x)), nrow = nr)      
  pred = matrix(sapply( pred, function(x) min(1-eps,x)), nrow = nr)
  ll = sum(act*log(pred) + (1-act)*log(1-pred))
  ll = ll * -1/(nrow(act))      
  return(ll);
}

pred1 = c(0.5,0.5)
pred2 = c(1.0,0.0)
pred <- rbind(pred1,pred2)
pred
act1 <- c(1,0)
act2 <- c(1,0)
act <- rbind(act1,act2)
MultiLogLoss(act,pred)

set.seed(1)

trainData <- read.csv("train.csv")
testData <- read.csv("test.csv")

trainRoughFix <- na.roughfix(trainData)
testRoughFix <- na.roughfix(testData)
trainRoughFixed <- trainRoughFix[,-127]
trainRoughFixed <- trainRoughFixed[,-115]
trainRoughFixed <- trainRoughFixed[,-58]
trainRoughFixed <- trainRoughFixed[,-24]
trainTrain = sample(1:nrow(trainRoughFixed), nrow(trainRoughFixed)/2)
actual.df <- data.frame(matrix(unlist(trainRoughFixed[-trainTrain,]$target), nrow=57161, byrow=T))
actual.df[,2] <- (1-actual.df[,1])

#baseline test if you guessed all 1's
allOnes.df <- data.frame()
allOnes.df[1:57161,1] <- 0.8
MultiLogLoss(actual.df, allOnes.df)
#8.18

#Random forests
#Bagged Tree with sqrt(p) predictors considered at each split, 500 trees
rf.bnp = randomForest(as.factor(target) ~ . - ID, data = trainRoughFixed, subset = trainTrain, mtry = 11, ntree = 500)

#analyze results
yhat.rf = predict(rf.bnp,newdata=trainRoughFixed[-trainTrain,],type="prob")
table(yhat.rf[,2] > .5, trainRoughFixed[-trainTrain,]$target)
'''
          0     1
  FALSE  2063  1008
  TRUE  11467 42623
  78.18% correct
'''
yhat.rflist <- yhat.rf[,2]
yhat.rfdf <- data.frame(matrix(unlist(yhat.rflist), nrow=57161, byrow=T))
MultiLogLoss(actual.df,yhat.rfdf)
#logloss = 8.18, something is wrong there idk what it is
#that is the same value as predicting all ones.


#n trees=3000, depth=3, shrinkage = 0.01
start.time <- Sys.time()
boost.bnp = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 3000, interaction.depth=3, shrinkage=0.01, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
best.iter <- gbm.perf(boost.bnp,method="OOB")
best.iter
yhat.boostn3d3s01cv5 <- predict(boost.bnp, newdata = trainRoughFixed[-trainTrain,], n.trees=best.iter, type='response')
table(yhat.boostn3d3s01cv5 > .5, trainRoughFixed[-trainTrain,]$target)
yhat.df <- data.frame(matrix(unlist(yhat.boostn3d3s01cv5), nrow=57161, byrow=T))
MultiLogLoss(actual.df,yhat.df)
#.823, its worse by a good amount


#Boosted Trees
#ntrees=3000, depth=4, shrinkage=0.01
start.time <- Sys.time()
boost.bnp = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 3000, interaction.depth=4, shrinkage=0.01, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
yhat.boostn3d4s01 <- predict(boost.bnp, newdata = trainRoughFixed[-trainTrain,], n.trees=best.iter, type='response')
table(yhat.boosted > .5, trainRoughFixed[-trainTrain,]$target)
yhat.df <- data.frame(matrix(unlist(yhat.boosted), nrow=57161, byrow=T))
MultiLogLoss(actual.df,yhat.boostn3d4s01)
#.8154 log loss


#n trees=1500, depth=5, shrinkage = 0.01
start.time <- Sys.time()
boost.bnp = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 1500, interaction.depth=5, shrinkage=0.01, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
best.iter <- gbm.perf(boost.bnp,method="OOB")
best.iter
yhat.boostn15d5s01 <- predict(boost.bnp, newdata = trainRoughFixed[-trainTrain,], n.trees=best.iter, type='response')
table(yhat.boostn15d5s01 > .5, trainRoughFixed[-trainTrain,]$target)
#78.25%
yhat.df <- data.frame(matrix(unlist(yhat.boostn15d5s01), nrow=57161, byrow=T))
MultiLogLoss(actual.df,yhat.df)
#.807, way better


#n trees=1300, depth=6, shrinkage = 0.01
start.time <- Sys.time()
boost.bnp = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 1300, interaction.depth=6, shrinkage=0.01, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
best.iter <- gbm.perf(boost.bnp,method="OOB")
best.iter
yhat.boostn13d6s01 <- predict(boost.bnp, newdata = trainRoughFixed[-trainTrain,], n.trees=best.iter, type='response')
table(yhat.boostn13d6s01 > .5, trainRoughFixed[-trainTrain,]$target)
#78.24%
yhat.df <- data.frame(matrix(unlist(yhat.boostn13d6s01), nrow=57161, byrow=T))
MultiLogLoss(actual.df,yhat.df)
#.794, a bit better


#n trees=1000, depth=7, shrinkage = 0.01
start.time <- Sys.time()
boost.bnp = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 1000, interaction.depth=7, shrinkage=0.01, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
best.iter <- gbm.perf(boost.bnp,method="OOB")
best.iter
yhat.boostn1d7s01 <- predict(boost.bnp, newdata = trainRoughFixed[-trainTrain,], n.trees=best.iter, type='response')
table(yhat.boostn1d7s01 > .5, trainRoughFixed[-trainTrain,]$target)
#78.28%
(42476+2274)/57161
yhat.df <- data.frame(matrix(unlist(yhat.boostn1d7s01), nrow=57161, byrow=T))
MultiLogLoss(actual.df,yhat.df)
#.8034, a bit worse

#depth 6 it is then. Shrinking parameter tunage


#n trees=1800, depth=6, shrinkage = 0.05
start.time <- Sys.time()
boost.bnp = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 1800, interaction.depth=6, shrinkage=0.05, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
best.iter <- gbm.perf(boost.bnp,method="OOB")
best.iter
yhat.boostn18d6s05 <- predict(boost.bnp, newdata = trainRoughFixed[-trainTrain,], n.trees=best.iter, type='response')
table(yhat.boostn18d6s05 > .5, trainRoughFixed[-trainTrain,]$target)
#78.29%
yhat.df <- data.frame(matrix(unlist(yhat.boostn18d6s05), nrow=57161, byrow=T))
MultiLogLoss(actual.df,yhat.df)
#.813, a bit worse, go other direction of shrinkage param


#n trees=1500, depth=6, shrinkage = 0.005
start.time <- Sys.time()
boost.bnp = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 1800, interaction.depth=6, shrinkage=0.005, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
best.iter <- gbm.perf(boost.bnp,method="OOB")
best.iter
yhat.boostd6s005 <- predict(boost.bnp, newdata = trainRoughFixed[-trainTrain,], n.trees=best.iter, type='response')
table(yhat.boostd6s005 > .5, trainRoughFixed[-trainTrain,]$target)
#78.29%
yhat.df <- data.frame(matrix(unlist(yhat.boostd6s005), nrow=57161, byrow=T))
MultiLogLoss(actual.df,yhat.df)
#0.799, a little bit better than 0.05 but worse than 0.01


#try one more time with 0.001
#else than try a submission with boosted tree n>3000, d=6, s=0.001
#run during class


#n trees=4000, depth=6, shrinkage = 0.001
start.time <- Sys.time()
boost.bnp = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 4000, interaction.depth=6, shrinkage=0.001, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
best.iter <- gbm.perf(boost.bnp,method="OOB")
best.iter
yhat.boostd6s005 <- predict(boost.bnp, newdata = trainRoughFixed[-trainTrain,], n.trees=best.iter, type='response')
table(yhat.boostd6s005 > .5, trainRoughFixed[-trainTrain,]$target)
(1977+42698)/57161
#78.15%
yhat.df <- data.frame(matrix(unlist(yhat.boostd6s005), nrow=57161, byrow=T))
MultiLogLoss(actual.df,yhat.df)
#0.731, a dramatic increase and n trees is still too low


#n trees=7000, depth=6, shrinkage = 0.001
start.time <- Sys.time()
boost.bnp = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 7000, interaction.depth=6, shrinkage=0.001, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
best.iter <- gbm.perf(boost.bnp,method="OOB")
best.iter
yhat.boostd6s001 <- predict(boost.bnp, newdata = trainRoughFixed[-trainTrain,], n.trees=best.iter, type='response')
table(yhat.boostd6s001 > .5, trainRoughFixed[-trainTrain,]$target)
(2283+42474)/57161
#78.30%
yhat.df <- data.frame(matrix(unlist(yhat.boostd6s001), nrow=57161, byrow=T))
MultiLogLoss(actual.df,yhat.df)
#0.7975, a dramatic increase and n trees is still too low
#on that same alg with ntrees=4000 ... 
yhat.boostn2d6s001 <- predict(boost.bnp, newdata = trainRoughFixed[-trainTrain,], n.trees=100, type='response')
table(yhat.boostn2d6s001 > .5, trainRoughFixed[-trainTrain,]$target)
(43302+1300)/57161
yhat.df <- data.frame(matrix(unlist(yhat.boostn2d6s001), nrow=57161, byrow=T))
MultiLogLoss(actual.df,yhat.df)
#0.7297, best one yet, close to one before
listLLs <- c()
for (i in 1:70) 
{
  yhat.forloop <- predict(boost.bnp, newdata = trainRoughFixed[-trainTrain,], n.trees=(i * 4000), type='response')
  yhat.df <- data.frame(matrix(unlist(yhat.forloop), nrow=57161, byrow=T))
  ll <- MultiLogLoss(actual.df,yhat.df)
  listLLs <- c(listLLs, ll);
}
listLLs
#Log loss appearently increases steadily as n trees increases.


#n trees=10000, depth=6, shrinkage = 0.001
start.time <- Sys.time()
boost.bnp = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 10000, interaction.depth=6, shrinkage=0.001, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
best.iter <- gbm.perf(boost.bnp,method="OOB")
best.iter
yhat.boostd6s005 <- predict(boost.bnp, newdata = trainRoughFixed[-trainTrain,], n.trees=best.iter, type='response')
table(yhat.boostd6s005 > .5, trainRoughFixed[-trainTrain,]$target)
(2307+42449)/57161
#78.30%
yhat.df <- data.frame(matrix(unlist(yhat.boostd6s005), nrow=57161, byrow=T))
yhat.df[,2] <- (1-yhat.df[,1])
MultiLogLoss(actual.df,yhat.df)
#.808

#partial for loop to check to make sure the log loss increases with tree size
listLLs <- c()
for (i in 1:10) 
{
  yhat.forloop <- predict(boost.bnp, newdata = trainRoughFixed[-trainTrain,], n.trees=(i * 100), type='response')
  yhat.df <- data.frame(matrix(unlist(yhat.forloop), nrow=57161, byrow=T))
  ll <- MultiLogLoss(actual.df,yhat.df)
  listLLs <- c(listLLs, ll);
}
listLLs
#if it does, then look at the predict values, not just the table
yhat.lowtrees <- predict(boost.bnp, newdata = trainRoughFixed[-trainTrain,], n.trees=400, type='response')
yhat.df2 <- data.frame(matrix(unlist(yhat.lowtrees), nrow=57161, byrow=T))
yhat.df2[,2] <- (1-yhat.df2[,1])
MultiLogLoss(actual.df,yhat.df2)

yhat.lowtrees[1:10]







start.time <- Sys.time()
boost.bnp = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 1000, interaction.depth=7, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
best.iter <- gbm.perf(boost.bnp,method="OOB")
best.iter
yhat.boosted = predict(boost.bnp, newdata = trainRoughFixed[-trainTrain,], n.trees=best.iter, type='response')
yhat.boosted[1:10]
table(yhat.boosted > .5,trainRoughFixed[-trainTrain,]$target)
#    0     1
#0 2208   1104
#1 11322 42527
#
#2208/3312 = 66.67% no's right
#42527/53849 = 78.97% yes's right
#78.26% overall correct

#n trees=6000, depth=3, shrinkage = 0.001
start.time <- Sys.time()
boost.bnp = gbm(target ~ . - ID, data = trainRoughFixed[trainTrain,], distribution = "bernoulli", n.trees = 6000, interaction.depth=3, verbose = TRUE)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
best.iter <- gbm.perf(boost.bnp,method="OOB")
best.iter
yhat.boosted = predict(boost.bnp, newdata = trainRoughFixed[-trainTrain,], n.trees=best.iter, type='response')
table(yhat.boosted > .5,trainRoughFixed[-trainTrain,]$target)
#    0     1
#0 2126   1031
#1 11404 42600
#
#2126/3157 = 67.34% no's right
#42600/54004 = 78.88% yes's right
#44726/57161 = 78.24% overall correct
yhat.df <- data.frame(matrix(unlist(yhat.boosted), nrow=57161, byrow=T))
MultiLogLoss(actual.df,yhat.df)
cor(yhat.boosted,yhat.rf[,2])
