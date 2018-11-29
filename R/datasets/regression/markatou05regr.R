# 见1150页底部

markatou05regr.DataGenerator <- function(dataConf){
  alpha <- 2
  beta <- 3
  bound.left <- 0
  bound.right <- 10
  if(!is.null(dataConf$alpha)) 
    alpha <- dataConf$alpha    
  if(!is.null(dataConf$beta))
    beta <- dataConf$beta
  if(!is.null(dataConf$left))
    bound.left <- dataConf$left
  if(!is.null(dataConf$right))
    bound.left <- dataConf$right
  n.size <- dataConf$nsize
  x <- runif(n.size, bound.left, bound.right)
  epsilon <- rnorm(n.size)
  y <- alpha + beta *x +epsilon
  return(data.frame(cbind(x,y)))
}

markatou05regr.Prepackages <- c()

markatou05regr.validation <- function(dataConf) {
  if(is.null(dataConf$nsize))
    return(F)
  return(T)
}