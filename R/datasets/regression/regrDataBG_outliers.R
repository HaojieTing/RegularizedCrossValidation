regrDataBG_outliers.DataGenerator<-function(dataConf){
  n <- dataConf$n
  d <- dataConf$d
  p <- 0.95
  if(!is.null(dataConf$p)) {
    p <- dataConf$p
  }
  const.var.1 <- 1
  const.var.2 <- 100
  if(!is.null(dataConf$var1)) {
    const.var.1 <- dataConf$var1
  }
  if(!is.null(dataConf$var2)) {
    const.var.2 <- dataConf$var2
  }
  mean<-rep(0,d)
  var1<-const.var.1 * diag(d)
  var2<-const.var.2 * diag(d)
  emean<-0
  evar1<-1/(const.var.1 * p+const.var.2 * (1-p))
  evar2<-const.var.2 * var1
  coef<-rep(sqrt(3/(d*(const.var.1 * p+const.var.2 * (1-p)))),d)
  t1<-rbinom(n,1,p)
  x<-matrix(rep(NA,n*d),n)
  y<-rep(NA,n)
  for(j in 1:n){
    if(t1[j]==1){
      x[j,]<-mvrnorm(1,mean,var1)
      y[j]<-coef%*%x[j,]+rnorm(1,emean,sqrt(evar1))
    }
    else{
      x[j,]<-mvrnorm(1,mean,var2)
      y[j]<-coef%*%x[j,]+rnorm(1,emean,sqrt(evar2))
    }
  }
  return(as.data.frame(cbind(x,y)))
}


regrDataBG_outliers.Prepackages <- c("MASS")

regrDataBG_outliers.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(F)
  if(is.null(dataConf$d)) return(F)
  return(T)
}

comment(regrDataBG_outliers.DataGenerator) <- c("")