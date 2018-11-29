maxClassifier.fit <- function(data_train) {
  y <- data_train[, ncol(data_train)]
  return(names(sort(summary(factor(y)),decreasing = TRUE)[1])[1])
}


maxClassifier.predict <- function(fit, data_test) {
  size <- nrow(data_test)
  pre <- rep(fit, size)
  return(pre)
}

maxClassifier.TrainAndTest <- function(data_train, data_test, algorConf) {
  model <- maxClassifier.fit(data_train)
  pre <- maxClassifier.predict(model, data_test)
  return(pre)
}


maxClassifer.Prepackages <- c()

maxClassifier.validation <- function(algorConf) {
  return(TRUE)
}

