# 数据来自:
#     Nadeau, C., & Bengio, Y. (2003). Inference for the Generalization Error. Machine Learning, 52(3), 239-281. doi: 10.1023/a:1024068626366
# 第259页，letter数据。
#     Bengio, Y., & Grandvalet, Y. (2004). No Unbiased Estimator of the Variance of K-Fold Cross-Validation. J. Mach. Learn. Res., 5, 1089-1105. 
# 第1102页，Experiment 3.
#
# url: http://archive.ics.uci.edu/ml/datasets/Letter+Recognition
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/5/30

uci_letter.DataGenerator <- function(dataConf) {
  letter.data <- GetExternalDataSet('letter')
  letter.data.size <- nrow(letter.data)
  letter.data.dim <- ncol(letter.data)
  letter.y <- letter.data[,1]
  letter.x.all <- letter.data[,2:letter.data.dim]
  binarize <- dataConf$binarize
  if(! is.null(binarize ) && binarize == TRUE) {
    class.zero.indices <- which (c(letter.y) <= 13)
    class.one.indices  <- which(c(letter.y) > 13) 
    letter.y <- c(letter.y)
    letter.y[class.zero.indices] <- 0
    letter.y[class.one.indices]  <- 1
  }
  letter.y <- factor(letter.y)
  letter.data.set <- as.data.frame(cbind(letter.x.all, letter.y))
  if(!is.null(dataConf$samplingConf)) {
    resample.config <- dataConf$samplingConf
    observants <- ResampleObservantsFromPopulationWithAdvancement(letter.data.set, resample.config)
    return(observants)
  } 
  return(letter.data.set)
}


uci_letter.PrePackages <- c()


uci_letter.validation <- function(dataConf) {
  if(is.null(dataConf$binarize)) {
    warning("provide a not binarized letter data set.")
  }
  if(is.null(dataConf$samplingConf)) {
    warning("provide whole data set without resampling.")
  }
  return(TRUE)
}

