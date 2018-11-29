# UCI Artificial 数据集.
# 
# 该数据集的URL为: https://archive.ics.uci.edu/ml/datasets/Artificial+Characters
# 该数据集被用在显著性检验方法的对照中。 
#   1. Yildiz, 《Omnivariate rule induction using a novel pairwise statistical 
#      test》.
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/5/30

uci_artificial.DataGenerator <- function(dataset.conf){
  dataset <- GetExternalDataSet("artificial")
  # 略去第一列和第三列
  dataset <- dataset[,-c(1,3)]
  dataset[,ncol(dataset)] <- factor(dataset[,ncol(dataset)])
  if(!is.null(dataset.conf$samplingConf)) {
    resample.config <- dataset.conf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(dataset, resample.config)
    return(observants)
  } 
  return(dataset)
}


uci_artificial.Prepackages <- c()


uci_artificial.validation <- function(dataset.conf) {
  return(TRUE)
}
