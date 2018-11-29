# UCI haberman
# Author: wang ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_haberman.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet('haberman', sep=",")
  data.set[, 4] <- factor(data.set[,4])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_haberman.PrePackages <- c()

uci_haberman.validation <- function(dataset.conf) {
  return(TRUE)
}