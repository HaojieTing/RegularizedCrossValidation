rpart.fit<-function(data_train, algorConf) {
  require(rpart)    
  fit <- NULL
  if("class" == algorConf$method) {    
    fit <- rpart(as.formula(paste(colnames(data_train)[ncol(data_train)], '~.', sep="")), data = data_train, control = NULL, method="class")    
  } else if("anova" == algorConf$method) {    
    data_train[,ncol(data_train)] <- as.numeric(data_train[,ncol(data_train)])-1
    fit<-rpart(as.formula(paste(colnames(data_train)[ncol(data_train)], '~.', sep="")), data = data_train, method="anova")
  } else {
    stop("other method config for classification tree is not support")
  }
  return(fit)
}


rpart.predict<-function(fit, data_test, algorConf){
  require(rpart)  
  pre <- NULL
  method <- algorConf$method
  if("class" == method) {   
    pre<-stats::predict(fit, data_test, type="class")    
  } else if("anova" == method) {  
    pre_tmp<-predict(fit, data_test)
    pre <- rep(NA, length(pre_tmp))
    for(i in 1:length(pre_tmp)) {
      if(is.na(pre_tmp[i])) pre_tmp[i] <- 0
      if(pre_tmp[i]>0.5)   { pre[i] <- 1 }
      else if(pre_tmp[i]<=0.5) { pre[i] <- 0 }
    }
  } else {
    stop(paste("not support:", method))
  }  
  return(factor(pre))
}


classificationTree.TrainAndTest <- function(data_train, data_test, algorConf) {
  
  model <- rpart.fit(data_train, algorConf)
  pre <-rpart.predict (model, data_test, algorConf)
  return(pre)
}



classificationTree.Prepackages <- c("rpart")


classificationTree.validation <- function(algorConf) {
  if(is.null(algorConf$method)) return(FALSE)
  return(TRUE)
}