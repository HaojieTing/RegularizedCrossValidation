lda.fit<-function(data_train, algorConf) {
  fit<-lda(as.formula(paste(colnames(data_train)[ncol(data_train)], '~.', sep="")), data = data_train)
  return(fit)
}


lda.predict<-function(model,data_test, algorConf){
  pre<-stats::predict(model,data_test)$class
  return(pre)
}


LinearDiscriminantAnalysis.TrainAndTest <- function(data_train, data_test, algorConf) {
  model <- lda.fit(data_train, algorConf)
  pre <-lda.predict (model, data_test, algorConf)
  return(pre)
}



LinearDiscriminantAnalysis.Prepackages <- c("MASS")


LinearDiscriminantAnalysis.validation <- function(algorConf) {return(TRUE)}
