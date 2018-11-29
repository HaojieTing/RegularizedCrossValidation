# UCI monks
# Author: wang ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_monk.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet('monk-2', sep=",")
  data.set[, 7] <- factor(data.set[,7])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_monk.PrePackages <- c()

uci_monk.validation <- function(dataset.conf) {
  return(TRUE)
}