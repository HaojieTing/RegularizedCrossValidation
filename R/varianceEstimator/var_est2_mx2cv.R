var_est2_mx2cv.EstForOneExpr <- function(mu, veConf) {
  m         <- veConf$m
  if(length(mu) != 2 *m +1)
    stop(paste(":",mu, m))
  mu_all <- mu[2*m+1]
  est = 0
  for(i in 1:(2*m)){
    est = est + (mu[i]-mu_all) ** 2
  }
  est = ((m+1)/(2*m*(m-1))) * est
  return(est)
}

var_est2_mx2cv.getfreedegree <- function(m) {
   return(2*m-1)
}

var_est2_mx2cv.Estimator <- function(veConf) {
  data_file <- veConf$data_file
  m         <- veConf$m
  if( !file.exists(data_file) ) {
    stop("the given data file does not exist.")
  }
  load( data_file )  # load muv.
  muv_est <- rep(NA, nrow(muv))
  for(i in 1:length(muv_est)){
    muv_est[i] <- var_est2_mx2cv.EstForOneExpr(muv[i,], m, veConf)
  }
  return(muv_est)
}


var_est2_mx2cv.validation <- function(veConf) {
  if(is.null(veConf$m)) return(FALSE)
  return(TRUE)
}
