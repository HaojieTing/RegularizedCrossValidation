# uci credit数据集
# Author: 王瑞波
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_credit.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet('credit',sep=",")
  data.set[, 16] <- factor(data.set[, 16])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_credit.PrePackages <- c()

uci_credit.validation <- function(dataset.conf) {
  return(TRUE)
}