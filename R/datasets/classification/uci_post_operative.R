# UCI postoprative数据集
# 
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_post_operative.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet('post-operative', sep=",")
  data.set[,9] <- factor(data.set[,9])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_post_operative.PrePackages <- c()

uci_post_operative.validation <- function(dataset.conf){
  return(TRUE)
}