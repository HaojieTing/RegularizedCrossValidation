svm.fit<-function(data_train, algorConf) {
  fit<-svm(as.formula(paste(colnames(data_train)[ncol(data_train)], '~.', sep="")), data=data_train, kernel="sigmoid")
  return(fit)
}


svm.predict<-function(fit, data_test, algorConf){
  pre<-stats::predict(fit,data_test,type="class", decision.values=TRUE)
  return(as.character(pre))
}


svm.TrainAndTest <- function(data_train, data_test, algorConf) {
  model <- svm.fit(data_train, algorConf)
  pre <-svm.predict (model, data_test, algorConf)
  return(c(pre))
}



svm.Prepackages <- c("e1071")


svm.validation <- function(algorConf) {
  return(TRUE)
}