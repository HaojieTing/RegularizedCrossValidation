# uci wine quality 数据集
#
# url: https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/
#
# Author: wang ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/11/8

uci_wine_quality.DataGenerator <- function(dataset.conf) {
  file_name = "winequality-white.csv"
  if(!is.null(dataset.conf$data_type) && dataset.conf$data_type=="red") {
    file_name = "winequality-red.csv"
  }
  data.set <- GetExternalDataSet(foldername = "UCI_ML_DataFolders/wine-quality",
                                 filename = file_name, sep=";", header=T)
  data.set[,ncol(data.set)] <- factor(data.set[,ncol(data.set)])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_wine_quality.PrePackages <- c()

uci_wine_quality.validation <- function(dataset.conf){
  return(TRUE)
}