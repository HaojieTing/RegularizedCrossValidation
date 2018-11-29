var_est_dietterich_mx2cv.EstForOneExpr <- function(mu, veConf) {
  m         <- veConf$m
  if ( length(mu) != 2*m + 1)
    stop(paste(":", length(mu), m))
  est = 0
  for(i in 1:m){
    mu_i = (mu[2*i-1] + mu[2*i]) /2
    est = est+ (mu[2*i-1]-mu_i)**2 + (mu[2*i]-mu_i)**2 
  }
  
  est = est/m
  return(est)
}


var_est_dietterich_mx2cv.Estimator <- function(veConf) {
  data_file <- veConf$data_file
  m         <- veConf$m
  if( !file.exists(data_file) ) {
    stop("the given data file does not exist.")
  }
  load( data_file )  # load muv.
  muv_est <- rep(NA, nrow(muv))
  for(i in 1:length(muv_est)){
    muv_est[i] <- var_est_dietterich_mx2cv.EstForOneExpr (muv[i,], m, veConf)
  }
  return(muv_est)
}


var_est_dietterich_mx2cv.validation <- function(veConf) {
  if(is.null(veConf$m)) return(FALSE)
  return(TRUE)
}