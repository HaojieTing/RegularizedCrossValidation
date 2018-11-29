#invoke Linear Regression algorithm from WEKA package.

WEKAClassifiers.fit <- function(datatrain, algorConf) {
  name <- algorConf$name
  classifier <- get(name)
  fit <- classifier(as.formula(paste(colnames(data_train)[ncol(data_train)], '~.', sep="")), data = data_train) 
  return(fit)
}

WEKAClassifiers.predict <- function(fit, data_test, algorConf) {
  result <- predict(fit, data_test, type='class')
  return(result)
}

WEKAClassifiers.TrainAndTest <- function(data_train, data_test, algorConf){
  algorname <- algorConf$name
  model <- get(paste(algorname,"fit", sep="."))(data_train, algorConf)
  pre <- get(paste(algorname,"predict", sep="."))(model, data_test, algorConf)
  return(pre)
}

WEKAClassifiers.Prepackages <- c('RWeka')

WEKAClassifiers.validation <- function(algorConf) {
  type <- algorConf$type
  name <- algorConf$name
  if("classification" == type) {
    classifiers <- c("Logistic", "SMO", "IBK", "LBR", "AdaBoostM1", "Bagging", "LogitBoost", "MultiBoostAB",
                     "Stacking", "CostSensitiveClassifier", "JRip", "M5Rules", "OneR", "PART", "J48", "LMT","M5P","DecisionStump")
    if(name %in% classifiers) {
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  else if("regression" == type) {
    classifiers <- c("LinearRegression")
    if(name %in% classifiers) {
      return(TRUE)
    }else{
      return(FALSE)
    }
  } else {
    return(FALSE)
  }
}

WEKAClassifiers.replaceVarNames <- function(algorname) {  
  assign(paste(algorname, "fit", sep="."), weka_dataset_loader.fit, envir = globalenv())
  remove(weka_dataset_loader.fit, envir = globalenv())
  assign(paste(algorname, "predict", sep="."), weka_dataset_loader.predict, envir = globalenv())
  remove(weka_dataset_loader.predict, envir = globalenv())
  assign(paste(algorname, "TrainAndTest", sep="."), weka_dataset_loader.TrainAndTest, envir = globalenv())
  remove(weka_dataset_loader.TrainAndTest, envir = globalenv())
  assign(paste(algorname, "Prepackages", sep="."), weka_dataset_loader.Prepackages, envir = globalenv())
  remove(weka_dataset_loader.Prepackages, envir = globalenv())
}


