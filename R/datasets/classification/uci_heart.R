# heart statlog 数据集
#
# url: http://archive.ics.uci.edu/ml/datasets/Statlog+%28Heart%29
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/6/4

uci_heart.DataGenerator <- function(dataConf) {
  heart.data.set <- GetExternalDataSet('heart_statlog', header = TRUE)
  heart.y<-heart.data.set[,14]
  heart.y <- factor(heart.y)
  heart.x.all<-heart.data.set[,-14]
  heart.data.set <- as.data.frame(cbind(heart.x.all, heart.y))
  if(!is.null(dataConf$samplingConf)) {
    resample.config <- dataConf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(heart.data.set, resample.config)
    return(observants)
  } 
  return(heart.data.set)
}


uci_heart.Prepackages <- c()


uci_heart.validation <- function(dataConf) {
  return(TRUE)
}

