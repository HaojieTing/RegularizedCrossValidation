# UCI hepatitis
# Author: wang ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_hepatitis.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet('hepatitis', sep=",")
  data.set[, ncol(data.set)] <- factor(data.set[,ncol(data.set)])
  data.set[which(data.set=="?", arr.ind = TRUE)] = NA
  data.set <- na.omit(data.set)
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_hepatitis.PrePackages <- c()

uci_hepatitis.validation <- function(dataset.conf) {
  return(TRUE)
}