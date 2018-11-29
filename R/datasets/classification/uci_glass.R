# glass 数据集
#
# url: http://archive.ics.uci.edu/ml/datasets/Glass+Identification
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/5/9
# Update：
#   1. wrb@2017/10/25 修正了glass读取函数。


uci_glass.DataGenerator <- function(dataConf){
  glass.data.set <- GetExternalDataSet('glass', header = TRUE)
  glass.y <- glass.data.set[,10]
  glass.y <- factor(glass.y)
  glass.x.all <- glass.data.set[, 1:9]
  glass.data.set <- as.data.frame(cbind(glass.x.all, glass.y))
  if(!is.null(dataConf$samplingConf)) {
    resample.config <- dataConf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(glass.data.set, resample.config)
    return(observants)
  } 
  return(glass.data.set)
}


uci_glass.Prepackages <- c()


uci_glass.validation <- function(dataConf) {
  return(TRUE)
}
