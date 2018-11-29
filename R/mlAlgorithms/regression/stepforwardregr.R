stepforward.fit <- function(data_train, algorConf) {
  min_form <- as.formula(paste(colnames(data_train)[ncol(data_train)], '~1', sep=""))
  max_form <- as.formula(paste(colnames(data_train)[ncol(data_train)], '~.', sep = ""))
  min.model <- lm(min_form, data = data_train)
  fwd.model = step(min.model, direction='forward', scope=max_form)
  return(fwd.model)
}


stepforward.predict <- function(fit, data_test, algorConf) {
  pre <- predict.lm(fit, newdata=data_test)
  return(pre)  
}


stepforwardregr.TrainAndTest <- function(data_train, data_test, algorConf) {
  model <- stepforward.fit(data_train, algorConf)
  pre <- stepforward.predict(model, data_test, algorConf)
  return(pre)
}



stepforwardregr.Prepackages <- c()



stepforwardregr.validation <- function(algorConf) {
  return(TRUE)
}
