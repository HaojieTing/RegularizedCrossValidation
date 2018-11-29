GaussianSample_indep_onehalf.DataGenerator <- function(number, d){
  mean <- rep(0, d)
  cov <- diag(d)
  coef <- rep(1.5, d+1)
  x <- mvrnorm(number, mean, cov)
  y <- rep(NA,number)
  xx<-cbind(1,x)
  nepe<-0
  p<-exp(xx%*%coef)/(1+exp(xx%*%coef))
  for(i in 1:number){
    y[i]<-rbinom(1,1,p[i])
    if(round(p[i])!=y[i])  nepe<-nepe+1
  }
  return(list(x,y,nepe))
}