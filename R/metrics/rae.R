# 回归问题的指标: relative absolute error
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2018/4/10


rae.Generator <- function(metric.conf) {
  response.gold <- metric.conf$gold
  response.pre  <- metric.conf$pre
  if (!is.numeric(response.gold)&&!is.numeric(response.pre)) {
    stop("In one-zero loss, resp or gold is not a valid type")
  }
  gold.mean <- mean(response.gold)
  denominator <- mean(abs(response.gold - gold.mean))
  rae.vector <- abs(response.pre - response.gold)/denominator
  rae.est <- mean(rae.vector)
  return(list(rae.est, rae.vector))
}  

rae.Prepackages <- c()

rae.validation <- function(metric.conf) {
  if(is.null(metric.conf$gold)||is.null(metric.conf$pre)) {
    return(FALSE)
  }
  return(TRUE)
}