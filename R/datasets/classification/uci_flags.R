# UCI flags数据集
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_flags.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet(filename = "flags", foldername = "UCI_ML_DataFolders/flags", sep=",")
  y <- factor(data.set[,7])
  data.set <- cbind(data.set[,-7], y)
  data.set <- data.set[,-c(1, 17, 28, 29)]
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_flags.PrePackages <- c()

uci_flags.validation <- function(dataset.conf){
  return(TRUE)
}