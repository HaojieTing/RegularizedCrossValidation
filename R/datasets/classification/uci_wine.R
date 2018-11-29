# wine数据集
#
# url: http://archive.ics.uci.edu/ml/datasets/Wine
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/6/4

uci_wine.DataGenerator <- function(dataConf){
  wine.data.set <- GetExternalDataSet('wine', sep = ",")
  wine.x.all <- wine.data.set[, -1]
  wine.y <- wine.data.set[, 1]
  wine.y <- factor(wine.y)
  wine.data.set <- as.data.frame(cbind(wine.x.all, wine.y))
  if(!is.null(dataConf$samplingConf)) {
    resample.config <- dataConf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(wine.data.set, resample.config)
    return(observants)
  } 
  return(wine.data.set)
}


uci_wine.Prepackages <- c()


uci_wine.validation <- function(dataConf) {
  return(TRUE)
}

