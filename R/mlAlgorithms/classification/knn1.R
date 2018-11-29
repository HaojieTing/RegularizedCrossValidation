knn1.TrainAndTest <- function(data_train, data_test, algorConf) {
  
  xtrain <- data_train[,1:(ncol(data_train)-1)]
  for( i in 1:ncol(xtrain)) {
    xtrain[,i] <- as.numeric(xtrain[,i])
  }
  xtest  <- data_train[,1:(ncol(data_test)-1)]
  for( i in 1:ncol(xtest)) {
    xtest[,i] <- as.numeric(xtest[,i])
  }
  y      <- data_train[,ncol(data_train)]
  if(is.factor(y))
    y <- as.factor(y)
  return(knn1(xtrain, xtest, y))  
}

knn1.Prepackages <- c('class')

knn1.validation <- function(algorConf) {return(TRUE)}