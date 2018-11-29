lm.fit<-function(data_train, algorConf){
  omittingIntercept <- algorConf$no_intercept 
  fom_str <- ""
  if ( omittingIntercept ) {
    fom_str <- paste(colnames(data_train)[ncol(data_train)], '~.-1', sep="")
  } else {
    fom_str <- paste(colnames(data_train)[ncol(data_train)], '~.', sep="")
  }
  formu <- as.formula(fom_str)
  md <- lm(formu,data=data_train)
  rm_vars <- names(which(is.na(md$coefficients)))
  while (length(rm_vars) > 0) {
     str <- ""
     for(var in rm_vars) {
       str <- paste(str, var, sep="-")
     }
     if ( omittingIntercept ) {
       fom_str <- paste(colnames(data_train)[ncol(data_train)], '~.-1', str, sep="")
     } else {
       fom_str <- paste(colnames(data_train)[ncol(data_train)], '~.', str, sep="")
     }
     formu <- as.formula(fom_str)
     md <- lm(formu,data=data_train)
     rm_vars <- names(which(is.na(md$coefficients)))
  }
  return(md)
}


lm.predict<-function(fit, data_test, algorConf){  
  omittingIntercept <- algorConf$no_intercept
  pre <- predict.lm(fit, newdata=data_test)
  return(c(pre))  
}


linearModel.TrainAndTest <- function(data_train, data_test, algorConf) {
  model <- lm.fit(data_train, algorConf)
  pre <- lm.predict(model, data_test, algorConf)
  return(pre)
}



linearModel.Prepackages <- c()



linearModel.validation <- function(algorConf) {
  if( is.null(algorConf$no_intercept) ) return(FALSE)
  return(TRUE)
}


