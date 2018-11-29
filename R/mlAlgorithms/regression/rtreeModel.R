rtree.fit<-function(data_train, algor.conf){
  fit <- rpart(y~., data= data_train, method="anova")
  return(fit)
}


rtree.predict<-function(fit, data_test, algor.conf){  
  require(rpart)
  pre <- predict(fit, data_test)
  return(c(pre))
}


rtreeModel.TrainAndTest <- function(data_train, data_test, algorConf) {
  model <- rtree.fit(data_train, algorConf)
  pre <- rtree.predict(model, data_test, algorConf)
  return(pre)
}



rtreeModel.Prepackages <- c("rpart")


rtreeModel.validation <- function(algorConf) {
  return(TRUE)
}