var_est1_mx2cv.EstForOneExpr <- function(mu, veConf) {
  m         <- veConf$m
  if(length(mu) != 2 *m +1)
    stop(paste(":",mu, m))
  est = 0
  for(i in 1:m){
    mu_i = (mu[2*i-1] + mu[2*i]) /2
    est = est + (mu[2*i-1]-mu_i)**2 + (mu[2*i] - mu_i)**2 
  }  
  est = est / (2*m)
  return(est)
}

var_est1_mx2cv.getfreedegree <- function(m) {
  return(m)
}

var_est1_mx2cv.Estimator <- function(veConf) {
  data_file <- veConf$data_file
  m         <- veConf$m
  if( !file.exists(data_file) ) {
    stop("the given data file does not exist.")
  }
  load( data_file )  # load muv.
  muv_est <- rep(NA, nrow(muv))
  for(i in 1:length(muv_est)){
    muv_est[i] <- var_est1_mx2cv.EstForOneExpr(muv[i,], m, veConf)
  }
  if(!is.null(conf_TASK_VAR_EST$multi) && conf_TASK_VAR_EST$multi == TRUE) {
    muv_est <- matrix(NA, nrow(muv), m-2+1)
    m_index <- 2
    while(m_index <= m) {
        tmuv <- muv[, 1:(2*m_index)]
        tmuv <- cbind(tmuv, rowMeans(tmuv))
        for(i in 1:nrow(tmuv)){
          muv_est[i,m_index-1] <- var_est1_mx2cv.EstForOneExpr(tmuv[i,], m_index, veConf)
        }        
    }
  }
  return(muv_est)
}


var_est1_mx2cv.validation <- function(veConf) {
  if(is.null(veConf$m)) return(FALSE)
  return(TRUE)
}