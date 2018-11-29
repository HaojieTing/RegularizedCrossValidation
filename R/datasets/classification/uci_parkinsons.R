# UCI parkinsons data set
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/27

uci_parkinsons.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet("parkinsons", sep=",", header=T)
  y <- data.set$status
  x <- data.set[,-match("status", names(data.set))]
  y <- factor(y)
  data.set <- as.data.frame(cbind(x,y))
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_parkinsons.PrePackages <- c()

uci_parkinsons.validation <- function(dataset.conf){
  return(TRUE)
}