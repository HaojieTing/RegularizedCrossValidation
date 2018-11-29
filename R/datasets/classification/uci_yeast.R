# yeast数据集
#
# url: http://archive.ics.uci.edu/ml/datasets/Yeast
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/6/4


uci_yeast.DataGenerator <- function(dataConf){
  yeast.data.set <- GetExternalDataSet('yeast')
  yeast.x.all <- yeast.data.set[, 2:9]
  yeast.y <- yeast.data.set[, 10]
  binarize <- dataConf$binarize
  if(! is.null(binarize) && binarize == TRUE) {
    yeast.y <- as.character(yeast.y)
    yeast.y[which(yeast.y=="CYT")] <- 1
    yeast.y[which(yeast.y=="ERL")] <- 1
    yeast.y[which(yeast.y=="EXC")] <- 1
    yeast.y[which(yeast.y=="ME1")] <- 1
    yeast.y[which(yeast.y=="ME2")] <- 1
    yeast.y[which(yeast.y=="ME3")] <- 1
    yeast.y[which(yeast.y=="MIT")] <- 0
    yeast.y[which(yeast.y=="NUC")] <- 0
    yeast.y[which(yeast.y=="POX")] <- 0
    yeast.y[which(yeast.y=="VAC")] <- 0
    yeast.y <- as.integer(yeast.y)
  }
  yeast.y <- factor(yeast.y)
  yeast.data.set <- as.data.frame(cbind(yeast.x.all, yeast.y))
  if(!is.null(dataConf$samplingConf)) {
    resample.config <- dataConf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(yeast.data.set, resample.config)
    return(observants)
  } 
  return(yeast.data.set)
}


uci_yeast.Prepackages <- c()


uci_yeast.validation <- function(dataConf) {
  if(is.null(dataConf$binarize)) {
    warning("provide a not binarized yeast data set.")
  }
  if(is.null(dataConf$samplingConf)) {
    warning("provide whole data set without resampling.")
  }
  return(TRUE)
} 