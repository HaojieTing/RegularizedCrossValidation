# UCI magic data set
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/27

uci_magic.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet(foldername = "UCI_ML_DataFolders/magic",
                                 filename = "magic04.data", sep=",")
  data.set[,ncol(data.set)] <- factor(data.set[,ncol(data.set)])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_magic.PrePackages <- c()

uci_magic.validation <- function(dataset.conf){
  return(TRUE)
}