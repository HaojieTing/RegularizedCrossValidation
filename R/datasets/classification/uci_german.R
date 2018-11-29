# UCI German数据集
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_german.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet('german',sep=",")
  data.set[,21] <- factor(data.set[,21])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_german.PrePackages <- c()

uci_german.validation <- function(dataset.conf) {
  return(TRUE)
}