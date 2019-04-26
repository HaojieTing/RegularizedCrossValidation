uci_diabetes.DataGenerator <- function(dataConf){
  diabetes.data.set <- GetExternalDataSet('diabetes', header = T)
  diabetes.x.all <- diabetes.data.set[, -9]
  diabetes.y     <- diabetes.data.set[, 9]
  diabetes.y <- factor(diabetes.y)
  diabetes.data.set <- as.data.frame(cbind(diabetes.x.all, diabetes.y)) 
  if(!is.null(dataConf$samplingConf)) {
    resample.config <- dataConf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(diabetes.data.set, resample.config)
    return(observants)
  } 
  return(diabetes.data.set)
}


uci_diabetes.Prepackages <- c()


uci_diabetes.validation <- function(dataConf) {
  return(TRUE)
}