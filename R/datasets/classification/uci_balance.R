# balance 数据集
#
# url: http://archive.ics.uci.edu/ml/datasets/Balance+Scale
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/6/4

uci_balance.DataGenerator <- function(dataConf) {
  balance.data.set <- GetExternalDataSet('balance_scale',header = TRUE)
  balance.x.all<-balance.data.set[,-5]
  balance.y<-balance.data.set[,5]
  balance.y <- factor(balance.y)
  balance.data.set <- as.data.frame(cbind(balance.x.all, balance.y))
  if(!is.null(dataConf$samplingConf)) {
    resample.config <- dataConf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(balance.data.set, resample.config)
    return(observants)
  } 
  return(balance.data.set)
}


uci_balance.PrePackages <- c()


uci_balance.validation <- function(dataConf) {
  return(TRUE)
}
