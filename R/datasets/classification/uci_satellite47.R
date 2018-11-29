# uci satellite 数据集
# url: https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/satimage/
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/11/8

uci_satellite47.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet(foldername = "UCI_ML_DataFolders/statlog/satimage",
                                 filename = "satellite47")
  data.set[,ncol(data.set)] <- factor(data.set[,ncol(data.set)])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_satellite47.PrePackages <- c()

uci_satellite47.validation <- function(dataset.conf){
  return(TRUE)
}