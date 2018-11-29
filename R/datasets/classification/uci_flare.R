# UCI solar-flare数据集
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/10/25

uci_flare.DataGenerator <- function(dataset.conf) {
  data.set <- GetExternalDataSet(filename = 'solar-flare', foldername = "UCI_ML_DataFolders/solar-flare")
  class_name <- "C"
  if(!is.null(dataset.conf$class_name)) {
    t_cname <- dataset.conf$class_name
    if(t_cname %in% c("C", "M", "X")) {
      class_name = t_cname
    }
  }
  class_index = 11
  if(class_name == "M")
    class_index = 12
  if(class_name == "X")
    class_index = 13
  y <- data.set[,class_index]
  x <- data.set[,-c(11,12,13)]
  y <- factor(y)
  data.set <- cbind(x,y)
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_flare.PrePackages <- c()

uci_flare.validation <- function(dataset.conf){
  return(TRUE)
}