# 具有四次切分的BRHS估计的方差估计。


var_est_brhs4.EstForOneExpr <- function(mu, veConf) {
  if(length(mu) != 5) stop("Length of mu is not correct.")
  mu.vec <- mu[1:4]
  mu.diff <- mu[5]
  ve.val <- 0
  for(idx in 1:4) {
    ve.val <- ve.val + (mu.vec[idx]-mu.diff)^2
  }
  ve.val <- 0.75 * ve.val
  return(ve.val)
}

var_est_brhs4.validation <- function(veConf) {
  return(TRUE)
}