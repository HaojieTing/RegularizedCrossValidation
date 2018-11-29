# UCI vote数据集
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_vote.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet('voting', sep=",")
  data.set[which(data.set=='?', arr.ind = TRUE)] = NA
  if (!is.null(dataset.conf$omit_na) && dataset.conf$omit_na== T ) {
    data.set <- na.omit(data.set)
  }
  y <- factor(data.set[,1])
  data.set <- cbind(data.set[,2:17], y)
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_vote.PrePackages <- c()

uci_vote.validation <- function(dataset.conf){
  return(TRUE)
}