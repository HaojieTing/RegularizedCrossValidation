# uci pendigits 数据集
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/27

uci_pendigits.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet(foldername = "UCI_ML_DataFolders/pendigits",
                                 filename = "pendigits", sep=",")
  data.set[,ncol(data.set)] <- factor(data.set[,ncol(data.set)])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_pendigits.PrePackages <- c()

uci_pendigits.validation <- function(dataset.conf){
  return(TRUE)
}