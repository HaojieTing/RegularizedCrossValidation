# UCI ecoli数据集
# 
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_ecoli.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet('ecoli')
  data.set[,9] <- factor(data.set[,9])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_ecoli.PrePackages <- c()

uci_ecoli.validation <- function(dataset.conf){
  return(TRUE)
}