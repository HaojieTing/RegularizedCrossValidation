# 岭回归算法。
# 
# 使用MASS包中自带的lm.ridge算法。
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/6/6
#
# FIXBUGS:
#   1) 测试时的代码更新，原来使用的fit$coef有问题，应该换为coef(fit)
#
# TODO: wangruibo@2017/6/6: ridge包依赖于R-3以上的版本。


lmRidge.fit<-function(data_train, algorConf){
  lambda <- 1
  if(!is.null(algorConf$lambda)) lambda <- algorConf$lambda
  fomu <- as.formula(paste(colnames(data_train)[ncol(data_train)], '~.', sep=""))
  return(lm.ridge(fomu, data=data_train, lambda = lambda))
}


lmRidge.predict<-function(fit, data_test, algorConf){  
  x <- as.matrix(data_test)
  x <- x[,-ncol(x)]
  if(fit$Inter == TRUE) {
    x <- cbind(rep(1, nrow(data_test)), x)
  }
  xcol = ncol(x)
  coefs <- coef(fit) 
  if(is.null(xcol) )
    xcol=length(x)
  if(xcol!= length(coefs))
    stop("length problem")
  return(c(x%*%coefs))
}


lmRidgeModel.TrainAndTest <- function(data_train, data_test, algorConf) {
  model <- lmRidge.fit(data_train, algorConf)
  pre <- lmRidge.predict(model, data_test, algorConf)
  return(pre)
}


lmRidgeModel.Prepackages <- c("MASS")



lmRidgeModel.validation <- function(algorConf) {
  return(TRUE)
}
