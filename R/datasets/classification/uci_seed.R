# uci seed数据集.
#
# url: http://archive.ics.uci.edu/ml/datasets/seeds
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/6/4

uci_seed.DataGenerator <- function(dataConf){
  seed.data.set <- GetExternalDataSet('seed')
  seed.x.all <- seed[, -8]
  seed.y <- seed[, 8]
  seed.y <- factor(seed.y)
  seed.data.set <- as.data.frame(cbind(seed.x.all, seed.y))
  if(!is.null(dataConf$samplingConf)) {
    resample.config <- dataConf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(seed.data.set, resample.config)
    return(observants)
  } 
  return(seed.data.set)
}

uci_seed.Prepackages <- c()


uci_seed.validation <- function(dataConf) {
  return(TRUE)
}