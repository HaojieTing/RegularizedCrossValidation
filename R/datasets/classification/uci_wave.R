# uci wave数据集
# url: https://archive.ics.uci.edu/ml/machine-learning-databases/waveform/
#
# Author: Wang ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/11/8

uci_wave.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet(foldername = "UCI_ML_DataFolders/waveform",
                                 filename = "waveform-+noise.data", sep=",")
  data.set[,ncol(data.set)] <- factor(data.set[,ncol(data.set)])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_wave.PrePackages <- c()

uci_wave.validation <- function(dataset.conf){
  return(TRUE)
}