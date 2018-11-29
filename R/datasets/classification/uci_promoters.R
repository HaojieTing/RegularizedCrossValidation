# UCI promoters数据集
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_promoters.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet('promoters')
  data.set <- data.set[, -1]
  data.set[,ncol(data.set)] <- factor(data.set[,ncol(data.set)])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_promoters.PrePackages <- c()

uci_promoters.validation <- function(dataset.conf){
  return(TRUE)
}