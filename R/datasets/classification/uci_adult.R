uci_adult.DataGenerator <- function(dataConf) {
  n <- dataConf$n #data size
  uci.adult.data <- loadPopulationDataFromExternal("adult", na.strings = "?", omitNArow=TRUE)
  observants <- GenerateObservantsWithReplace(uci.adult.data, n, y_balance = TRUE)
  return(observants)
}


uci_adult.Prepackages <- c()


uci_adult.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(FALSE)
  return(TRUE)
} 
