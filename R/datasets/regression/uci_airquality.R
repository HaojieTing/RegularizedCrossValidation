# 空气质量数据集
#
# url: http://archive.ics.uci.edu/ml/datasets/Air+Quality
#
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/6/3


uci_airquality.DataGenerator <- function(dataConf) {
  #n <- dataConf$n #data size
  #uci.airquality.data <- loadPopulationDataFromExternal("airquality", na.strings = c("-200","-200.0"), omitNArow=FALSE)
  #去掉变量1,2,5
  uci.airquality.data <- GetExternalDataSet("airquality", header = T)
  uci.airquality.data <- uci.airquality.data[,c(-1,-2)]
  data.size <- nrow(uci.airquality.data)
  data.y <- uci.airquality.data[,ncol(uci.airquality.data)]
  data.x.all <- uci.airquality.data[,1:(ncol(uci.airquality.data)-1)]
  data.set <- as.data.frame(cbind(data.x.all, data.y))
  if(!is.null(dataConf$samplingConf)) {
    resample.config <- dataConf$samplingConf  # ybalance不能为TRUE
    observants <- ResampleObservantsFromPopulationWithAdvancement(data.set, resample.config)
    return(observants)
  } 
  return(data.set)
}

uci_airquality.Prepackages <- c()

uci_airquality.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(FALSE)
  return(TRUE)
} 
