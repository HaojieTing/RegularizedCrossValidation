sm.fit <- function(data_train, algorConf) {
  y <- as.vector(data_train[,ncol(data_train)])
  return(mean(y))
}


sm.predict <- function(model, data_test, algorConf) {
  simple_count <- nrow(data_test)
  if(is.null(simple_count)){
    simple_count <- length(xtest)
  }  
  return(rep(model, simple_count))
}


simpleMeanRegression.TrainAndTest <- function(data_train, data_test, algorConf) {
  model <- sm.fit(data_train, algorConf)
  pre <- sm.predict(model, data_test, algorConf)
  return(pre)
}

simpleMeanRegression.Prepackages <- c()


simpleMeanRegression.validation <- function(algorConf) {
  return(TRUE)
}