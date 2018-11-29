# UCI blood-trasfusion数据集
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_donors.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet('blood-transfusion', sep=",", header = T)
  data.set[,ncol(data.set)] <- factor(data.set[,ncol(data.set)])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_donors.PrePackages <- c()

uci_donors.validation <- function(dataset.conf){
  return(TRUE)
}