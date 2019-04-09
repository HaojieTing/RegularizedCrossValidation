tree.fit <-function(data_train, algorConf)
{
  fit<-tree(as.formula(paste(colnames(data_train)[ncol(data_train)], '~.', sep="")), data_train)
  return(fit)
}

tree.predict<-function(fit, data_test, algorConf)
{
  data <- data_test
  pre <- predict(fit, data, type="class")
  
  return(pre)
}


tree.TrainAndTest <- function(data_train,data_test, algorConf) { 
  model <- tree.fit(data_train, algorConf)
  pre   <- tree.predict(model, data_test, algorConf)
  return(pre)
}

tree.Prepackages <- c("tree")

tree.validation <- function(algorConf) {
  return(TRUE)
}

