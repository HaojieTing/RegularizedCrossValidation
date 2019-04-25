
regressionTree.fit <-function(data_train, algorConf)
{
  if(!is.null(algorConf$test_method) && algorConf$test_method=="regression") {
    # 只是用level为0,1的二分类
    data_train[,ncol(data_train)] <- as.numeric(data_train[,ncol(data_train)])-1
  }
  fit<-tree(as.formula(paste(colnames(data_train)[ncol(data_train)], '~.', sep="")), data_train)
  return(fit)
}


regressionTree.predict<-function(fit, data_test, algorConf)
{
  data <- data_test
  pre <- NA
  if( algorConf$test_method == "class" ){
    pre <- predict(fit, data, type=algorConf$test_method)
  } else if ( algorConf$test_method == "regression" ) {
    pre  <- predict(fit, data)
    pre  <- as.numeric(pre > 0.5)
  }  
  return(pre)
}


regressionTree.TrainAndTest <- function(data_train,data_test, algorConf) { 
  model <- regressionTree.fit(data_train, algorConf)
  pre   <- regressionTree.predict(model, data_test, algorConf)
  return(pre)
}



regressionTree.Prepackages <- c("tree")


regressionTree.validation <- function(algorConf) {
  if( is.null(algorConf$test_method) ) {
    warning("test_method is not set.")
    return(FALSE)
  }
  return(TRUE)
}

