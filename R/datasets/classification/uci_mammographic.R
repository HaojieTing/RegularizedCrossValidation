# UCI mammographic data set
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/27

uci_mammographic.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet('mammographic', sep=",")
  data.set[which(data.set=='?', arr.ind = T)] = NA
  if( !is.null(dataset.conf$omit_na) && dataset.conf$omit_na == T) {
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

uci_mammographic.PrePackages <- c()

uci_mammographic.validation <- function(dataset.conf){
  return(TRUE)
}