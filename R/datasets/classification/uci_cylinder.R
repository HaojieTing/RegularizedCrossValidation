# UCI cylinder数据集
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_cylinder.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet(filename = 'cylinder-bands', foldername = 'UCI_ML_DataFolders/cylinder-bands', sep=",")
  data.set <- data.set[,-1] # 去掉时间戳
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

uci_cylinder.PrePackages <- c()

uci_cylinder.validation <- function(dataset.conf){
  return(TRUE)
}