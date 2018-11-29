# UCI bupa数据集
# url: 
# Author: wangruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_bupa.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet("bupa")
  data.set[,7] <- factor(data.set[,7])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(as.data.frame(data.set))
}

uci_bupa.PrePackages <- c()

uci_bupa.validatioin <- function(dataset.conf) {
  return(TRUE)
}