# mxK折交叉验证估计的方差估计。
#
# 估计的具体形式是：单次K交叉验证估计做样本方差估计，然后
# 除以K, 最后将m个方差估计平均。
#

var_est_corrected_mxkcv.EstForOneExpr <- function(mu, m, veConf) {
  vector.length <- length(mu)
  if((vector.length-1)%%m != 0 || vector.length -1 == m)
    stop(paste("invalid vector length"))
  K <- (vector.length-1)/m
  estimator.matrix <- matrix(mu[1:(length(mu)-1)], nrow = m)
  sample.variances.holdout <- apply(estimator.matrix, 1, var)
  rho_hat <- 1/K
  divider <- m*K*(1-rho_hat)/(1+(K-1)*rho_hat)
  sample.variances.kfcv  <- sample.variances.holdout/divider
  variance.final <- mean(sample.variances.kfcv)
  return(variance.final)
}


var_est_corrected_mxkcv.Estimator <- function(veConf) {
  data_file <- veConf$data_file
  m         <- veConf$m
  if( !file.exists(data_file) ) {
    stop("the given data file does not exist.")
  }
  load( data_file )  # load muv.
  muv_est <- rep(NA, nrow(muv))
  for(i in 1:length(muv_est)){
    muv_est[i] <- var_est_sample_mxkcv.EstForOneExpr (muv[i,], m, veConf)
  }
  return(muv_est)
}


var_est_corrected_mxkcv.validation <- function(veConf) {
  if(is.null(veConf$m)) return(FALSE)
  return(TRUE)
}