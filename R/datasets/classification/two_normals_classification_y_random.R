two_normals_classification_y_random.DataGenerator <- function(dataConf){
  require(MASS)
  n <- dataConf$n
  y_p <- dataConf$y_p
  mu0 <- dataConf$mu0
  mu1 <- dataConf$mu1
  sigma0 <- dataConf$sigma0
  sigma1 <- dataConf$sigma1  
  n1 <- sum(rbinom(n = n, size = 1, prob = y_p))
  n0 <- n - n1
  x0<-mvrnorm(n0, mu0, sigma0)
  x1<-mvrnorm(n1, mu1, sigma1)
  x<-rbind(x0,x1)
  y<-c(rep(0, n0), rep(1, n1))
  list.xy <- list(x, y)
  names(list.xy) <- c('x', 'y')
  return(list.xy)
}


two_normals_classification_y_random.Prepackages <- c("MASS")


two_normals_classification_y_random.validation <- function(dataConf) {
  if(is.null(dataConf$n))    {  return(FALSE)  }
  if(is.null(dataConf$y_p))    { return(FALSE) }
  if(is.null(dataConf$mu0))   { return(FALSE)}
  if(is.null(dataConf$mu1))   { return(FALSE) }
  if(is.null(dataConf$sigma0)) {return(FALSE)}
  if(is.null(dataConf$sigma1)) {return(FALSE)}
  return(TRUE)
}

comment(two_normals_classification_y_random.DataGenerator) <- c()