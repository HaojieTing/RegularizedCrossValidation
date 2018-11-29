lda.fit<-function(data_train) {
  removeCols <- c()
  for(i in 1:ncol(x=data_train)) {
    
    if(1 == nlevels(factor(data_train[,i])) && i!=ncol(data_train)){
      removeCols <- c(removeCols, i)
    }
    if(is.factor(data_train[,i])){      
      data_train[,i] <- as.numeric(data_train[,i])
    }    
  } 
  data_train1 <- data_train[,-removeCols]
  fit <- NULL
  tryCatch(
    fit<-lda(as.formula(paste(colnames(data_train1)[ncol(data_train1)], '~.', sep="")), data = data_train1),
    error = function(e){
      msg <- e$message
      msg <- gsub("variable ", "", msg)
      msg <- gsub("appears to be constant within groups","", msg)
      removeCols <- c(removeCols, as.vector(as.numeric(msg)))
      data_train1 <- data_train[,-removeCols]
      fit<-lda(as.formula(paste(colnames(data_train1)[ncol(data_train1)], '~.', sep="")), data = data_train1)
    }
  )
  
  model <- list(fit=fit, remove=removeCols)
  return(model)
}


lda.predict<-function(model,data_test){
  fit <- model$fit
  removeCols <- model$remove
  for(i in 1:ncol(x=data_test)) {    
    if(is.factor(data_test[,i])){      
      data_test[,i] <- as.numeric(data_test[,i])
    }
  } 
  data_test <- data_test[,-removeCols]
  pre<-stats::predict(fit,data_test)$class
  return(pre)
}


LinearDiscriminantAnalysis.TrainAndTest <- function(data_train, data_test, algorConf) {
  model <- lda.fit(data_train)
  pre <-lda.predict (model, data_test)
  return(pre)
}



LinearDiscriminantAnalysis.Prepackages <- c("MASS")


LinearDiscriminantAnalysis.validation <- function(algorConf) {return(TRUE)}
