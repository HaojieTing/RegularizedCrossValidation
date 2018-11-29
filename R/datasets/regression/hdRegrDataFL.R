# 高维回归数据集
#
#
#
#
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/5/16

hdRegrDataFL.DataGenerator<-function(dataConf){ #n:sample size; d: predictor dimension
  n <- dataConf$n
  d <- dataConf$d  
  phi <- dataConf$phi  
  if( is.null(dataConf$phi))  phi <- 0.5
  beta <- dataConf$beta
  if( is.null(dataConf$beta)) beta <- 5
  covmat<-matrix(rep(phi, d*d), d)
  for(i in 1:d){
    for(j in 1:d){
      if(i==j)
        covmat[i,j]=1
      if((i==4 && j!=4) || (i!=4&&j==4))
        covmat[i,j]=sqrt(phi)
    } 
  }
  eps<-rnorm(n,0,1)
  mean<-rep(0,d)
  x <- mvrnorm(n, mean, covmat)  
  y<-beta * x[,1] + beta * x[,2] +beta * x[,3] - 3*beta*sqrt(phi)*x[,4] + eps
  return(as.data.frame(cbind(x,y)))
}


hdRegrDataFL.Prepackages <- c("MASS")


hdRegrDataFL.validation <- function(dataConf) {
  if (is.null(dataConf$n)) return(FALSE)
  if (is.null(dataConf$d)) return(FALSE)
  if (is.null(dataConf$phi)) {
    warning("the config term phi is not provided, default value is 0.5")
  }
  if (is.null(dataConf$beta)) {
    warning("the config term beta is not provided, default value is 5")
  }
  return(TRUE)
}
