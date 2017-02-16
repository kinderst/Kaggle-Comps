library(xgboost)
library(randomForest)

set.seed(2016)

trainRaw <- read.csv("train.csv", stringsAsFactors=TRUE) 
y <- trainRaw$target
trainRaw$target <- NULL
trainRaw$ID <- NULL
nrowTrain <- nrow(trainRaw)

testRaw <- read.csv("test.csv", stringsAsFactors=TRUE) 
testID <- testRaw$ID
testRaw$ID <- NULL

allData <- rbind(trainRaw, testRaw)
allData <- as.data.frame(allData)

#feat addition
N <- ncol(allData)
allData$NACount_N <- rowSums(is.na(allData)) / N 

featNames <- names(allData)

# make feature of counts of zeros factor
allData$ZeroCount <- rowSums(allData[,featNames]== 0) / N

for (f in featNames) {
  if (class(allData[[f]])=="character" || class(allData[[f]])=="factor") {
    allData[[f]] <- as.integer(factor(allData[[f]]))
  }
}

allData <- na.roughfix(allData)
colnames(allData)
train <- allData[1:nrowTrain,]
test <- allData[(nrowTrain+1):nrow(allData),]

xgtrain = xgb.DMatrix(as.matrix(train), label = y)
xgtest = xgb.DMatrix(as.matrix(test))

set.seed(2016)
results <- c()
loopCount <- 0
overallBest <- 9999
overallBestName <- ""

etas <- c(0.005)
gammas <- c(0)
maxDepths <- c(10)
#minChilds <- c(1)
#maxDeltas <- c(1)
#subsamples <- c(0.9)
#colSubsamples <- c(0.7)
start.time <- Sys.time()
for (eta in etas) {
  for (gamma in gammas) {
    for (maxDepth in maxDepths) {
      params0 <- list(
        # some generic, non specific params
        "objective"  = "binary:logistic"
        , "eval_metric" = "logloss"
        , "eta" = eta
        , "gamma" = gamma
        , "max_depth" = maxDepth
        , "min_child_weight" = 1
        , "max_delta_step" = 1
        , "subsample" = 0.9
        , "colsample_bytree" = 0.7
      )
      
      model_cv = xgb.cv(
        params = params0
        , nrounds = 20000
        , nfold = 2
        , nthread = 4
        , data = xgtrain
        , early.stop.round = 20
        , maximize = FALSE
      )
      best <- min(model_cv$test.logloss.mean)
      bestIter <- which(model_cv$test.logloss.mean==best)
      rm(model_cv)
      gc()
      
      if (best < overallBest) {
        overallBest <- best
        overallBestName <- paste("eta:", eta, "gamma:", gamma, "maxDepth:", maxDepth, "logloss:", best, "bestiter:", bestIter, sep = " ")
      }
      
      loopCount <- loopCount + 1
      cat(loopCount)
      
      results <- c(results, paste("eta:", eta, "gamma:", gamma, "maxDepth:", maxDepth, "logloss:", best, "bestiter:", bestIter, sep = " "))
    }
  }
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

results
overallBest
overallBestName

bestIter <- round(bestIter * 1.4)

ensemble <- rep(0, nrow(test))

doTest <- function(param0, iter) {
  watchlist <- list('train' = xgtrain)
  model = xgb.train(
    nrounds = iter
    , params = params0
    , data = xgtrain
    , watchlist = watchlist
    , print.every.n = 20
  )
  p <- predict(model, xgtest)
  rm(model)
  gc()
  p
}

for (i in 1:3) {
  print(i)
  set.seed(i + 2015)
  p <- doTest(params0, bestIter) 
  # use 40% to 50% more than the best iter rounds from your cross-fold number.
  # as you have another 50% training data now, which gives longer optimal training time
  ensemble <- ensemble + p
}

yhat = data.frame()
predicts <- ensemble/i
predicts <- data.frame(predicts)
write.csv(data.frame(ID = testID, PredictedProb = predicts), "xgboost_tree_benchmark4compare.csv", row.names = F)

xgb.importance(, model =)