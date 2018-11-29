# UCI page-block data set
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/27

uci_page_block.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet(foldername = "UCI_ML_DataFolders/page-blocks",
                                 filename = "page-blocks.data")
  data.set[,ncol(data.set)] <- factor(data.set[,ncol(data.set)])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_page_block.PrePackages <- c()

uci_page_block.validation <- function(dataset.conf){
  return(TRUE)
}