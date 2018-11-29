# 回归问题的指标: relative absolute error
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2018/4/10


rse.Generator <- function(metric.conf) {
  response.gold <- metric.conf$gold
  response.pre  <- metric.conf$pre
  if (!is.numeric(response.gold)&&!is.numeric(response.pre)) {
    stop("In one-zero loss, resp or gold is not a valid type")
  }
  gold.mean <- mean(response.gold)
  denominator <- mean((response.gold - gold.mean)^2)
  rse.vector <- (response.pre - response.gold)^2/denominator
  rse.est <- mean(rse.vector)
  return(list(rse.est, rse.vector))
}  

rse.Prepackages <- c()

rse.validation <- function(metric.conf) {
  if(is.null(metric.conf$gold)||is.null(metric.conf$pre)) {
    return(FALSE)
  }
  return(TRUE)
}