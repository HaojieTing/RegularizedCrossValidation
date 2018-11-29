knn.TrainAndTest <- function(data_train, data_test, algorConf) {
  k <- algorConf$k
  kernal <- algorConf$kernal
  data_train <- na.omit(data_train)
  del_c <- c()
  for( i in 1:(ncol(data_train)-1)) {
    if(length(levels(factor(data_train[, i]))) ==1)
      del_c <- c(del_c, i)
  } 
  if(is.null(del_c) || length(del_c) == 0) {
    
  }else   data_train <- data_train[, -del_c]
  fit <- kknn(as.formula(paste(colnames(data_train)[ncol(data_train)], '~.', sep="")),data_train, data_test, k=k, kernel=kernal)
  pre <- fitted(fit)
  return(pre)
}


knn.Prepackages <- c("kknn")


knn.validation <- function(algorConf) {
  if(is.null(algorConf$k)) {
    stop()
  }
  if(is.null(algorConf$kernal)){
    stop("knn type：triangular, rectangular")
  }
  return(TRUE)
}