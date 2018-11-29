sample_variance_estimator_vfcv.EstForOneExpr <- function(mu, veConf) {
  v <- veConf$v
  if(length(mu) != v + 1) {
    stop(paste(":", length(mu), v ))
  }
  return(var(mu[1:v])/v)
}

sample_variance_estimator_vfcv.Estimator <- function(veConf) {
  
  data_file <- veConf$data_file
  v         <- veConf$v
  if( !file.exists(data_file) ) {
    stop("the given data file does not exist.")
  }
  load( data_file )  # load muv.
  muv_est <- rep(NA, nrow(muv))
  for(i in 1:length(muv_est)){
    muv_est[i] <- sample_variance_estimator_vfcv.EstForOneExpr(muv[i,], v, veConf)
  }
  return(muv_est)
}


sample_variance_estimator_vfcv.validation <- function(veConf) {
  if(is.null(veConf$v)) return(FALSE)
  return(TRUE)
}

