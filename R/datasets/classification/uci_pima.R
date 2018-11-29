# pima数据集
#
# url: http://archive.ics.uci.edu/ml/datasets/Pima+Indians+Diabetes
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/6/4
uci_pima.DataGenerator <- function(dataConf) {
  pima.data <- GetExternalDataSet('pima', sep=",")
  pima.y <- pima.data[,9]
  pima.x.all <- pima.data[,1:8]
  pima.y <- factor(pima.y)
  pima.data.set <- as.data.frame(cbind(pima.x.all, pima.y))
  if(!is.null(dataConf$samplingConf)) {
    resample.config <- dataConf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(pima.data.set, resample.config)
    return(observants)
  } 
  return(pima.data.set)
}

uci_pima.Prepackages <- c()

uci_pima.validation <- function(dataConf) {
  return(TRUE)
} 

