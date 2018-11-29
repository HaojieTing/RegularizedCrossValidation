# uci zoo 数据集
# url: http://archive.ics.uci.edu/ml/machine-learning-databases/zoo/
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/11/8

uci_transfusion.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet(foldername = "UCI_ML_DataFolders/blood-transfusion",
                                 filename = "transfusion.data", sep=",", header = T)
  data.set[,ncol(data.set)] <- factor(data.set[,ncol(data.set)])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_transfusion.PrePackages <- c()

uci_transfusion.validation <- function(dataset.conf){
  return(TRUE)
}