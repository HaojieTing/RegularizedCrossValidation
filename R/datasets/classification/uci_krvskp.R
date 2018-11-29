# UCI chess kr vs. kp
# http://archive.ics.uci.edu/ml/machine-learning-databases/chess/king-rook-vs-king-pawn/
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/27

uci_krvskp.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet(foldername = "UCI_ML_DataFolders/chess/king-rook-vs-king-pawn",
                                 filename = "kr-vs-kp.data", sep=",")
  data.set[,ncol(data.set)] <- factor(data.set[,ncol(data.set)])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_krvskp.PrePackages <- c()

uci_krvskp.validation <- function(dataset.conf){
  return(TRUE)
}