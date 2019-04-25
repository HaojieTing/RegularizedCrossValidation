nnet.fit<-function(data_train, algorConf) {
  h_size <- algorConf$h_size  
  data_train[,ncol(data_train)] <- factor(data_train[,ncol(data_train)])
  data_train <- na.omit(data_train)
  formula <- as.formula(paste(colnames(data_train)[ncol(data_train)], '~.', sep=""))
  weights.count.max <- 10000
  if (!is.null(algorConf$MaxNwts)) {
    weights.count.max <- algorConf$MaxNwts
  }
  if (h_size != 0)
    fit<-nnet(formula, data=data_train, size=h_size, maxit=200, trace=FALSE)
  else 
    fit<-nnet(formula, data=data_train, size=0, maxit=200, trace=FALSE, skip=TRUE)
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
  if( is.null(algorConf$h_size) ) return(FALSE)
  if (is.null(algorConf$MaxNwts)) {
    warning("You could tune param MaxNwts when error 'too many weights' error is thorw out.")
  }
  return(TRUE)
}
