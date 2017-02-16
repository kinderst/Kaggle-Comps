library(data.table)
# Start the clock!
start_time <- t1 <- Sys.time()

na.roughfix2 <- function (object, ...) {
  res <- lapply(object, roughfix)
  structure(res, class = "data.frame", row.names = seq_len(nrow(object)))
}

roughfix <- function(x) {
  missing <- is.na(x)
  if (!any(missing)) return(x)
  
  if (is.numeric(x)) {
    x[missing] <- median.default(x[!missing])
  } else if (is.factor(x)) {
    freq <- table(x)
    x[missing] <- names(freq)[which.max(freq)]
  } else {
    stop("na.roughfix only works for numeric or factor")
  }
  x
}

# Set a seed for reproducibility
set.seed(2016)

cat("reading the train and test data\n")
# Read train and test
train_raw <- fread("train.csv", stringsAsFactors=TRUE) 
print(dim(train_raw))
print(sapply(train_raw, class))

y <- train_raw$target
train_raw$target <- NULL
train_raw$ID <- NULL
n <- nrow(train_raw)

test_raw <- fread("test.csv", stringsAsFactors=TRUE) 
test_id <- test_raw$ID
test_raw$ID <- NULL
print(dim(test_raw))
print(sapply(test_raw, class))
cat("Data read ")
print(difftime( Sys.time(), start_time, units = 'sec'))

# Preprocess data
# Find factor variables and translate to numeric
cat("Preprocess data\n")
all_data <- rbind(train_raw,test_raw)
all_data <- as.data.frame(all_data) # Convert data table to data frame

# Convert v22 to hexavigesimal base
az_to_int <- function(az) {
  xx <- strsplit(tolower(az), "")[[1]]
  pos <- match(xx, letters[(1:26)]) 
  result <- sum( pos* 26^rev(seq_along(xx)-1))
  return(result)
}

all_data$v22cvt<-sapply(all_data$v22, az_to_int)

feature.names <- names(all_data)

cat("assuming text variables are categorical & replacing them with numeric ids\n")
cat("re-factor categorical vars & replacing them with numeric ids\n")
for (f in feature.names) {
  if (class(all_data[[f]])=="character" || class(all_data[[f]])=="factor") {
    #levels <- unique(c(all_data[[f]]))
    #all_data[[f]] <- as.integer(factor(all_data[[f]], levels=levels))
    all_data[[f]] <- as.integer(factor(all_data[[f]]))
  }
}

# Small feature addition - Count NA percentage
N <- ncol(all_data)
all_data$NACount_N <- rowSums(is.na(all_data)) / N 

# make feature of counts of zeros factor
all_data$ZeroCount <- rowSums(all_data[,feature.names]== 0) / N
all_data$Below0Count <- rowSums(all_data[,feature.names] < 0) / N

feature.names <- names(all_data)

train <- all_data[1:n,]
test <- all_data[(n+1):nrow(all_data),] 

print(dim(train))
#summary(train)tr
print(dim(test))
#summary(test)

#rm(all_data)
#gc()

#Feature selection using KS test with 0.007 as cutoff.
tmpJ = 1:ncol(test)
ksMat = NULL
for (j in tmpJ) {
  cat(j," ")
  ksMat = rbind(ksMat, cbind(j, ks.test(train[,j],test[,j])$statistic))
}

ksMat2 = ksMat[ksMat[,2]<0.007,]
feats = as.numeric(ksMat2[,1]) 
cat(length(feats),"\n")
cat(names(train)[feats],"\n")