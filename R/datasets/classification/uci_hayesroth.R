# UCI hayesroth
#
# Author: wang ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_hayesroth.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet('hayes-roth', sep=",")
  data.set[, 6] <- factor(data.set[,6])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_hayesroth.PrePackages <- c()

uci_hayesroth.validation <- function(dataset.conf) {
  return(TRUE)
}