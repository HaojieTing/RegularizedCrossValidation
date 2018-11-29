var_est_general_mx2cv.EstForOneExpr <- function(mu, veConf){
  m         <- veConf$m
  lambda_1  <- veConf$lambda_1
  lambda_2  <- veConf$lambda_2
  if(length(mu) != 2 *m +1)
    stop(paste("给出的mu的长度不对：",mu, m))
  mu_i <- rep(NA, m)
  est1 = 0
  est2=0
  for(i in 1:m){
    mu_i[i] = (mu[2*i-1] + mu[2*i]) /2
    est1 =est1+ (mu[2*i-1]-mu_i[i])**2 + (mu[2*i]-mu_i[i])**2 
  }
  #est1 = est1 /(m**2)
  mu_all = mu[2*m+1]
  for(i in 1:m){
    est2 = est2+ (mu_i[i]-mu_all) ** 2
  }
  #est2 = est2 /(m-1)
  est = lambda_1 * est1 + lambda_2 * est2
  return(est)
}


var_est_general_mx2cv.Estimator <- function(veConf) {
  data_file <- veConf$data_file
  m         <- veConf$m
  lambda_1  <- veConf$lambda_1
  lambda_2  <- veConf$lambda_2
  if( !file.exists(data_file) ) {
    stop("the given data file does not exist.")
  }
  load( data_file )  # load muv.
  muv_est <- rep(NA, nrow(muv))
  for(i in 1:length(muv_est)){
    muv_est[i] <- var_est_general_mx2cv.EstForOneExpr(muv[i,], m, veConf)
  }
  if(!is.null(conf_TASK_VAR_EST$multi) && conf_TASK_VAR_EST$multi == TRUE) {
    muv_est <- matrix(NA, nrow(muv), m-2+1)
    m_index <- 2
    while(m_index <= m) {
      tmuv <- muv[, 1:(2*m_index)]
      tmuv <- cbind(tmuv, rowMeans(tmuv))
      for(i in 1:nrow(tmuv)){
        muv_est[i,m_index-1] <- var_est_general_mx2cv.EstForOneExpr(tmuv[i,], m_index, veConf)
      }        
    }
  }
  return(muv_est)
}


var_est_general_mx2cv.validation <- function(veConf) {
  if(is.null(veConf$m)) return(FALSE)
  return(TRUE)
}
