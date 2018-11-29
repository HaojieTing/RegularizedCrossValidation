
lrc.fit <- function(data_train, algorConf) {
  y <- data_train[, ncol(data_train)]
  if(!is.factor(y)) stop("need factor response")
  data_train[, ncol(data_train)] <- c(data_train[, ncol(data_train)])-1
  fit <- lm(as.formula(paste(colnames(data_train)[ncol(data_train)], '~.', sep="")), data = data_train)
  return( fit )
}


lrc.predict <- function(model, data_test, algorConf) {
  xtest <- data_test[,1:(ncol(data_test)-1)]
  for(i in 1: ncol(xtest)){
    xtest[,i] <- as.numeric(xtest[,i])
  }
  x <- as.matrix(xtest)
  pre <- cbind(1, x)%*%model$coef
  pre_bin <- as.integer(pre > 0.5)
  return(factor(pre_bin)) 
}

linearRegrClassifier.TrainAndTest <- function(data_train, data_test, algorConf) {
  model <- lrc.fit(data_train, algorConf)
  pre <- lrc.predict(model, data_test, algorConf)
  return(pre)
}


linearRegrClassifier.Prepackages <- c()


linearRegrClassifier.validation <- function(algorConf) {
  return(TRUE)    
}
