# UCI Australian数据集
#
# 数据集url: https://archive.ics.uci.edu/ml/datasets/Australian+Sign+Language+signs
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_australian.DataGenerator <- function(dataset.conf) {
  dataset <- GetExternalDataSet("australian", sep=",")
  dataset <- na.omit(dataset)
  if(!is.null(dataset$omit0x3F) && dataset$omit0x3F == TRUE) {
    dataset <- dataset[which(dataset[,15]!='0x3F'),]
  }
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(dataset, resample.config)
    return(observants)
  } 
  return(dataset)
}

uci_australian.PrePackages <- c()

uci_australian.validation <- function(dataset.conf) {
  return(TRUE)
}