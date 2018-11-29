# uci ironosphere 数据集
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_ironosphere.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet('ironosphere', header = T)
  data.set[,35] <- factor(data.set[,35])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_ironosphere.PrePackages <- c()

uci_ironosphere.validation <- function(dataset.conf){
  return(TRUE)
}