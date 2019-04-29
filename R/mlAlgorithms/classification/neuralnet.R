neuralnet.fit<-function(data_train, algorConf) {
  h_size <- algorConf$h_size  
  y = as.factor(data_train[,ncol(data_train)])
  x = data.frame(data_train[,1:(ncol(data_train)-1)])
  if (h_size != 0)
    fit<-nnet(y~.,x, size=h_size, maxit=200, trace=FALSE)
  else 
    fit<-nnet(y~.,x, size=0, maxit=200, trace=FALSE, skip=TRUE)
  return(fit)
}


neuralnet.predict<-function(fit,data_test, algorConf){
  pre<-stats::predict(fit, data_test, type="class")
  result <- factor(pre)
  return(pre)
}


neuralnet.TrainAndTest <- function(data_train, data_test, algorConf) {
  model <- neuralnet.fit(data_train, algorConf)
  pre <-neuralnet.predict (model, data_test, algorConf)
  return(pre)
}



neuralnet.Prepackages <- c("nnet")


neuralnet.validation <- function(algorConf) {
  if( is.null(algorConf$h_size) ) return(FALSE)
  if (is.null(algorConf$MaxNwts)) {
    warning("You could tune param MaxNwts when error 'too many weights' error is thorw out.")
  }
  return(TRUE)
}
