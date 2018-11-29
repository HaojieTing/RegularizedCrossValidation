bolassoreg.DataGenerator <- function(dataConf){
  if(!is.null(dataConf$seed))
    set.seed(dataConf$seed)
  n <- dataConf$n
  p <- dataConf$p
  r <- dataConf$r
  #truemodel <- dataConf$truemodel
  mu <- dataConf$mu
  
  w <- sample(c(1,-1), r, replace = T) * runif(r, 1/3, 1)
  w <- c(w, rep(0, p-r))
 
  G <- mvrnorm(p, rep(0,p), diag(p))
  Q <- G %*% t(G)
  corr <- t(scale(Q)/sqrt(p-1)) %*% (scale(Q)/sqrt(p-1))
  
  x <- mvrnorm(n, rep(mu,p), corr)
  sigma <- sqrt(mean((x %*% w)^2))
  y <- x %*% w + rnorm(n,0, 0.1*sigma)
  return(data.frame(x=x,y=y))
}


bolassoreg.Prepackages <- c("MASS")

bolassoreg.validation <- function(dataConf) {
  if( is.null(dataConf$n) ) return(FALSE) # 1000
  if( is.null(dataConf$p) ) return(FALSE) # 16
  if( is.null(dataConf$r) ) return(FALSE) # 8
  if( is.null(dataConf$mu) ) return(FALSE) # 0
  #if( is.null(dataConf$truemodel) ) return(FALSE)
  return(TRUE)
}


