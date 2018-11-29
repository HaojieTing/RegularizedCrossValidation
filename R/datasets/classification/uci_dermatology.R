# uci dermatology数据集
#
# Author: 王瑞波
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_dermatology.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet("dermatology",sep=",")
  data.set[,35] <- factor(data.set[,35])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_dermatology.PrePackages <- c()

uci_dermatology.validation <- function(dataset.conf) {
  return(TRUE)
}