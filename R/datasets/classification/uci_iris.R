# iris数据集
#
# url: http://archive.ics.uci.edu/ml/datasets/Iris
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/6/3

uci_iris.DataGenerator <- function(dataConf){
  iris.data.set <- GetExternalDataSet('iris', header = T)
  iris.x.all <- iris.data.set[, -5]
  iris.y     <- iris.data.set[, 5]
  iris.y <- factor(iris.y)
  iris.data.set <- as.data.frame(cbind(iris.x.all, iris.y)) 
  if(!is.null(dataConf$samplingConf)) {
    resample.config <- dataConf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(iris.data.set, resample.config)
    return(observants)
  } 
  return(iris.data.set)
}


uci_iris.Prepackages <- c()


uci_iris.validation <- function(dataConf) {
  return(TRUE)
}