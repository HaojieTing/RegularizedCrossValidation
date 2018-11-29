nnet.fit<-function(data_train, algorConf) {
  rang <- algorConf$range
  h_size <- algorConf$h_size  
  data_train[,ncol(data_train)] <- factor(data_train[,ncol(data_train)])
  data_train <- na.omit(data_train)

  del_c <- c()
  for( i in 1:(ncol(data_train)-1)) {
    if(length(levels(factor(data_train[, i]))) ==1)
      del_c <- c(del_c, i)
  } 
  if(is.null(del_c) || length(del_c) == 0) {
    
  }else   data_train <- data_train[, -del_c]
  formula <- as.formula(paste(colnames(data_train)[ncol(data_train)], '~.', sep=""))
  fit <- NULL
  weights.count.max <- 10000
  if (!is.null(algorConf$MaxNwts)) {
    weights.count.max <- algorConf$MaxNwts
  }
  if (h_size != 0)
    fit<-nnet(formula, data=data_train, size=h_size, maxit=200,rang = rang, MaxNWts = weights.count.max, trace=FALSE)
  else 
    fit<-nnet(formula, data=data_train, size=h_size, maxit=200,rang = rang, trace=FALSE, skip=TRUE)
  return(fit)
}


nnet.predict<-function(fit,data_test, algorConf){
  pre<-stats::predict(fit, data_test, type="class")
  result <- factor(pre)
  return(pre)
}


nnet.TrainAndTest <- function(data_train, data_test, algorConf) {
  model <- nnet.fit(data_train, algorConf)
  pre <-nnet.predict (model, data_test, algorConf)
  return(pre)
}



nnet.Prepackages <- c("nnet")


nnet.validation <- function(algorConf) {
  if( is.null(algorConf$range) ) return(FALSE)
  if( is.null(algorConf$h_size) ) return(FALSE)
  if (is.null(algorConf$MaxNwts)) {
    warning("You could tune param MaxNwts when error 'too many weights' error is thorw out.")
  }
  return(TRUE)
}
