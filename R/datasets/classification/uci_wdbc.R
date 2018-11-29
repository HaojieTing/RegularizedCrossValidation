# wdbc 数据集
#
# url: http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/6/4


uci_wdbc.DataGenerator <- function(dataConf){
  wdbc.data.set <- GetExternalDataSet('wdbc', sep=",")
  wdbc.x.all <- wdbc.data.set[, -31]
  wdbc.y <- wdbc.data.set[, 31]
  wdbc.y <- factor(wdbc.y)
  wdbc.data.set <- as.data.frame(cbind(wdbc.x.all, wdbc.y))
  if(!is.null(dataConf$samplingConf)) {
    resample.config <- dataConf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(wdbc.data.set, resample.config)
    return(observants)
  } 
  return(wdbc.data.set)
}


uci_wdbc.Prepackages <- c()

uci_wdbc.validation <- function(dataConf) {
  return(TRUE)
} 

