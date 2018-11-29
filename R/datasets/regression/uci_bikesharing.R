# 自行车共享数据
#
# url: http://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
#
# 这里仅适用day数据。
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/6/3

uci_bikesharing.DataGenerator <- function(dataConf) {
  bike.day.data <- GetExternalDataSet('bikesharing', header = T)
  # 去掉索引号和日期.
  bike.day.data <- bike.day.data[,3:ncol(bike.day.data)]
  bike.data.size <- nrow(bike.day.data)
  bike.data.y <- bike.day.data[,ncol(bike.day.data)]
  bike.data.x.all <- bike.day.data[,1:(ncol(bike.day.data)-1)]
  bike.data.set <- as.data.frame(cbind(bike.data.x.all, bike.data.y))
  if(!is.null(dataConf$samplingConf)) {
    resample.config <- dataConf$samplingConf  # ybalance不能为TRUE
    observants <- ResampleObservantsFromPopulationWithAdvancement(bike.data.set, resample.config)
    return(observants)
  } 
  return(bike.data.set)
}


uci_bikesharing.Prepackages <- c()


uci_bikesharing.validation <- function(dataConf) {
  return(TRUE)
} 