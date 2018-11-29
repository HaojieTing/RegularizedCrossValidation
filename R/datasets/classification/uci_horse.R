# UCI horse数据集
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/26

uci_horse.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet(foldername = "UCI_ML_DataFolders/horse-colic",
                                 filename = "horse-colic")
  data.set[which(data.set=="?", arr.ind = T)] <- NA # 获取缺省值
  if(!is.null(dataset.conf$omit_na) && dataset.conf$omit_na == T) {
    data.set <- na.omit(data.set)
  }
  data.set[,ncol(data.set)] <- factor(data.set[,ncol(data.set)])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_horse.PrePackages <- c()

uci_horse.validation <- function(dataset.conf){
  return(TRUE)
}