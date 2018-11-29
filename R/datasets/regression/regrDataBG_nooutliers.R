regrDataBG_nooutliers.DataGenerator<-function(dataConf){
  n <- dataConf$n
  d <- dataConf$d
  require("MASS")
  if(n<=0 || d<=0){
    stop("dimension or sampling number are less than zero")
  }
  mean<-rep(0,d)
  var<-diag(d)
  emean<-0
  evar<-1
  coef<-rep(sqrt(3/d),d)
  x<-mvrnorm(n,mean,var)
  y<-x%*%coef+rnorm(n,emean,evar)  
  return(as.data.frame(cbind(x,y)))
}


regrDataBG_nooutliers.Prepackages <- c("MASS")


regrDataBG_nooutliers.validation <- function(dataConf) {
  if( is.null(dataConf$n) ) return(FALSE)
  if( is.null(dataConf$d) ) return(FALSE)
  return(TRUE)
}


