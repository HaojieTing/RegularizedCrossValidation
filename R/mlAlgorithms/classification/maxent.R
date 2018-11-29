
maxent.fit <- function(data_train, algorConf) {
  col.count <- ncol(data_train)
  x <- data_train[,1:(col.count-1)]
  y <- data_train[, col.count]
  y <- as.factor(y)
  model <- maxent(x, y, l2_regularizer = 1.0)
  return(model)
}

maxent.predict <- function(model, data_test, algorConf) {
  col.count <- ncol(data_test)
  x <- data_test[,1:(col.count-1)]
  pre <- predict(model,x)
  return(pre)
}

maxent.TrainAndTest<- function(data_train, data_test, algorConf) {
  model <- maxent.fit(data_train, algorConf)
  pre <- maxent.predict(model, data_test, algorConf)
  return(pre[, 'labels'])
}

maxent.Prepackages <- c("maxent")


maxent.validation <- function(algorConf) {
  return(TRUE)
}
