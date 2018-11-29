# UCI cmc 数据集.
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_cmc.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet("cmc",sep = ",")
  data.set[,10] <- factor(data.set[,10])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_cmc.PrePackages <- c()

uci_cmc.validation <- function(dataset.conf) {
  return(TRUE)
}