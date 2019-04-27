# uci ironosphere 数据集
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_ionosphere.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet('ionosphere', header = T)
  ionosphere.x.all <- data.set[,1:34]
  ionosphere.x.all <- ionosphere.x.all[,-2]
  ionosphere.y <-  factor(data.set[,35])
  ionosphere.data.set <- as.data.frame(cbind(ionosphere.x.all, ionosphere.y))
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(ionosphere.data.set, resample.config)
    return(observants)
  } 
  return(ionosphere.data.set)
}

uci_ionosphere.PrePackages <- c()

uci_ionosphere.validation <- function(dataset.conf){
  return(TRUE)
}