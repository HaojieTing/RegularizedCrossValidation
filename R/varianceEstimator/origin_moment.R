origin_moment.EstForOneExpr <- function(mu, veConf) {  
  m <- veConf$m
  if(length(mu) != 2 *m +1)
    stop(paste("uncorrect：",mu, m))
  est = 0
  for(i in 1:2*m){    
    est = est + mu[i]^2 
  }  
  est = est / (2*m)
  return(est)
}

origin_moment.Estimator <- function(veConf) {
  data_file <- veConf$data_file
  m         <- veConf$m
  if( !file.exists(data_file) ) {
    stop("the given data file does not exist.")
  }
  load( data_file )  # load muv.
  muv_est <- rep(NA, nrow(muv))
  for(i in 1:length(muv_est)){
    muv_est[i] <- origin_moment.EstForOneExpr(muv[i,], m, veConf)
  }
  return(muv_est)
}


origin_moment.validation <- function(veConf) {
  if(is.null(veConf$m)) return(FALSE)
  return(TRUE)
}
