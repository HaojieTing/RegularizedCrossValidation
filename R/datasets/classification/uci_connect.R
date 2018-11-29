uci_connect.DataGenerator <- function(dataConf) {
  n <- dataConf$n #data size
  uci.connect.data <- loadPopulationDataFromExternal("connect-4", na.strings = "?", omitNArow=TRUE)
  observants <- GenerateObservantsWithReplace(uci.connect.data, n, y_balance = TRUE)
  return(observants)
}


uci_connect.Prepackages <- c()


uci_connect.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(FALSE)
  return(TRUE)
} 
