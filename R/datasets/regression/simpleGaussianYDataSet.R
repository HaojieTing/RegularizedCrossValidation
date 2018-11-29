#
#
#
#

simpleGaussianYDataSet.DataGenerator <- function(dataConf){
  mean.value <- dataConf$mu  # 理论均值
  var.value  <- dataConf$psi # 理论方差
  n.value <- dataConf$n    # 数据个数
  y <- rnorm(n = n.value, mean = mean.value, sd = sqrt(var.value))
  return(data.frame(rep(1, length(y)), y))
}

simpleGaussianYDataSet.Prepackages <- c()

simpleGaussianYDataSet.validation <- function(dataConf) {
  if (is.null(dataConf$n)) return(FALSE)
  if (is.null(dataConf$mu)) return(FALSE)
  if (is.null(dataConf$psi)) return(FALSE)
  return(TRUE)
}