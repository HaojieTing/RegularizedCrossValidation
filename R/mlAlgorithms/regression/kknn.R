kknn.train <- function(data.train, algor.conf) {
  print(data.train)
  model <- train.kknn(y~., data.train )
  return(model)
}

kknn.test <- function(model, data.test, algor.conf) {
  predictions <- predict(model, data.test)
  return(c(predictions))
}

kknn.TrainAndTest <- function(data_train, data_test, algorConf) {
  model <- kknn.train(data_train, algorConf)
  pre <- kknn.test(model, data_test, algorConf)
  return(pre)
}

kknn.Prepackages <- c("kknn")

kknn.validation <- function(algorConf) {
  return(T)
}