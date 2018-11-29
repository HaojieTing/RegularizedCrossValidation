# Lasso 回归算法模块。
#
# 使用lars包中提供的Lasso算法。
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2015/6/6
#
# 修改记录:
# wangruibo@2017/9/13: 放开lasso测试时需要的mode参数和s参数。
# wangruibo@2017/9/14: 在调试模式下,输出每个模型所用到的变量集合。

Lasso.fit<-function(data_train, algorConf){
  x <- as.matrix(data_train[,-c(ncol(data_train))])
  y <- as.vector(data_train[,ncol(data_train)])
  useGram <- algorConf$useGram
  if(is.null(useGram))
    useGram <- TRUE
  return(lars(x,y,type='lasso', use.Gram=useGram))
}


Lasso.predict<-function(fit, data_test, algorConf){  
  x <- as.matrix(data_test[,-c(ncol(data_test))])
  prop <- algorConf$prop
  mod <- "fraction"
  if(!is.null(algorConf$mod)) {
    mod <- algorConf$mod
  }
  if(!is.null(algorConf$debug) && algorConf$debug==TRUE) {
    # 当开启了算法的调试模式时，存储所选到的真实变量。
    if(!exists("debug_info")) {
      debug_info <<- list()
    }
    if(!is.list(debug_info)) {
      stop("Variable debug_info is not a list type.")
    }
    coef <- predict.lars(fit, s=prop, mode=mod)
    debug_info <<- c(debug_info, list(c(which(coef$coefficients!=0))))
  }
  pre <- predict.lars(fit, newx=x, type="fit", s=prop, mode=mod)
  return(c(pre$fit))
}


LassoModel.TrainAndTest <- function(data_train, data_test, algorConf) {
  model <- Lasso.fit(data_train, algorConf)
  pre <- Lasso.predict(model, data_test, algorConf)
  return(c(pre))
}

LassoModel.Prepackages <- c("lars")


LassoModel.validation <- function(algorConf) {
  valid <- TRUE
  if(is.null(algorConf$prop)) {
    warning("The prop config is not specified in Lasso Algorithm Modular.")
    valid <- FALSE
  }
  if(is.null(algorConf$mod)) {
    warning("The mode config is not specified in Lasso Algorithm Modular. Default is fraction")
  }
  return(valid)
}

