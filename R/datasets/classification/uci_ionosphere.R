# uci ironosphere 数据集
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_ionosphere.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet('ionosphere', header = T)
  data.set <- data.set[,-2]
  data.set[,35] <- factor(data.set[,35])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_ionosphere.PrePackages <- c()

uci_ionosphere.validation <- function(dataset.conf){
  return(TRUE)
}