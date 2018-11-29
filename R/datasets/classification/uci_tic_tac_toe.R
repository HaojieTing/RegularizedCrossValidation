# UCI tic-tac-toe 数据集
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_tic_tac_toe.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet('tic-tac-toe', sep=",")
  data.set[,ncol(data.set)] <- factor(data.set[,ncol(data.set)])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_tic_tac_toe.PrePackages <- c()

uci_tic_tac_toe.validation <- function(dataset.conf){
  return(TRUE)
}