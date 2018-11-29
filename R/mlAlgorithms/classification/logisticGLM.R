logisticglm.fit <- function(data_train, algorConf){
  if(nlevels(factor(data_train[, ncol(data_train)])) != 2) 
    stop("")
  fit <- glm(as.formula(paste(colnames(data_train)[ncol(data_train)], '~.', sep="")), data=data_train, family="binomial")
  return(fit)
}


logisticglm.predict <- function(fit, data_test, algorConf){ 
  coef <- as.vector(fit$coef)
  x <- data_test[, 1:(ncol(data_test)-1)]
  x <- cbind(1, x)
  x <- as.matrix(x)
  p <- exp( x %*% coef ) / ( 1 + exp(x%*%coef) )
  return(c(round(p)))
}


logisticGLM.TrainAndTest <- function(data_train, data_test, algorConf) {
  model <- logisticglm.fit(data_train, algorConf)
  return(logisticglm.predict(model, data_test, algorConf))
}


#给出分类器所需要载入的包
logisticGLM.Prepackages <- c()


logisticGLM.validation <- function(algorConf) {
  # 验证配置是否正确;
  #
  # Args:
  #   算法配置;
  #
  # Return:
  #   TURE: 配置正确; FALSE: 配置错误;
  return(TRUE)
}


comment(logisticGLM.TrainAndTest) <- c("广义线性模型中的两类logistic分类算法")
