# 数据来自：
#   Wang, Y., Wang, R., Jia, H., & Li, J. (2014). Blocked 3x2 Cross-Validated t-Test for Comparing Supervised Classification Learning Algorithms. Neural Computation, 26(1), 208-235. 
# 第216页 Simulated Experiment 2.
#


simWYneco2014Expr2.DataGenerator <- function(dataConf) {
  n <- dataConf$n
  d <- 5
  x <- matrix(NA, nrow = n, ncol = d)
  y <- sample(x = c(0,1), n, replace = T, prob = c(0.5, 0.5))
  cnt.y.pos <- length(which(y==1))
  cnt.y.neg <- n - cnt.y.pos
  x[which(y==1),] <- mvrnorm(n = cnt.y.pos, mu = rep(1, d), 2*diag(d))
  x[which(y==0),] <- mvrnorm(n = cnt.y.neg, mu = rep(0, d), diag(d))
  data <- as.data.frame(cbind(x, y))
  data[,ncol(data)] <- factor(data[,ncol(data)])
  return(data)
}


simWYneco2014Expr2.Prepackages <- c("MASS")


simWYneco2014Expr2.validation <- function( dataConf ) {
  if(is.null(dataConf$n)) return(FALSE)
  return(TRUE)
}


