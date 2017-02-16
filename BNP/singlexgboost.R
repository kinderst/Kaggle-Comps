#.45930 log loss, best one yet. Now time to tune
#library(data.table) #Faster reading
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


#possible param values
'
eta: 0.001, 0.01, 0.05, 0.1, 0.3, 0.5
step size shrinkage parameter. if set to a low value, generally need more n trees needed. controls overfitting, weights new trees, which controls the rate at which boosting learns. lower=underfitting, higher=overfitting
gamma: 0.1, 1, 10
Adds penalty for each split, try high penalties with high number of d splits, low penalties for low number of d splits (because this gamma parameter inherently limits the number of splits anyways)
max_depth: 7, 8, 9, 10, 11, 12, 13
tree depth size
min_child_weight: 0, 1 (default), 3, 5
if you want to do a split, sometimes there too few data points for the leaf, in that case its not a good idea to do a split
max_delta_step: 0 (default), 1, 2, 5
maximum delta step allowed for each trees weight estimation to be, helps predict right probably by helping convergence
subsample: 0.7, 0.85, 1 (default)
sample size of data for every new tree introduced. Robust to noise
colsample_bytree: 0.8, 0.9, 1
subsample of possible predictors on each split. Robust to noise
'


params0 <- list(
  # some generic, non specific params
  "objective"  = "binary:logistic"
  , "eval_metric" = "logloss"
  , "eta" = 0.04
  , "subsample" = 0.9
  , "colsample_bytree" = 0.9
  , "min_child_weight" = 1
  , "max_depth" = 10
)

model_cv = xgb.cv(
  params = params0
  , nrounds = 500
  , nfold = 2
  , data = xgtrain
  , early.stop.round = 20
  , maximize = FALSE
)
#155 too!
gc()
best <- min(model_cv$test.logloss.mean)
bestIter <- which(model_cv$test.logloss.mean==best)
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
write.csv(data.frame(ID = testID, PredictedProb = predicts), "xgboost_tree_benchmark3compare.csv", row.names = F)

xgb.importance(, model =)