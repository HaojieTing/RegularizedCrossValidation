RegData.DataGenerator <- function(nn, d, p){
    require("MASS")
    if(nn<=0 || d<=0){
      stop("dimension or sampling number are less than zero")
    }
    mean <- rep(0,d)
    var <- diag(d)
    emean <- rep(0,nn)
    evar <- matrix(rep(p,(nn)^2),nn)+(1-p)*diag(nn)
    coef <- rep(sqrt(3/d),d)
    x<-mvrnorm(nn,mean,var)
    print(x)
    epsilons <- matrix(mvrnorm(1,emean,evar),nn,1)
    print(epsilons)
    y <- x%*%coef+ epsilons
    return(list(x,y))
}


generRegData.DataGenerator<-function(n, d, p=0.3){
  Datalist1 <- RegData.DataGenerator((n/10),d,p) 
  Datalist2 <- RegData.DataGenerator((n/10),d,p)
  Datalist3 <- RegData.DataGenerator((n/10),d,p)
  Datalist4 <- RegData.DataGenerator((n/10),d,p)
  Datalist5 <- RegData.DataGenerator((n/10),d,p)
  Datalist6 <- RegData.DataGenerator((n/10),d,p)
  Datalist7 <- RegData.DataGenerator((n/10),d,p)
  Datalist8 <- RegData.DataGenerator((n/10),d,p)
  Datalist9 <- RegData.DataGenerator((n/10),d,p)
  Datalist10 <- RegData.DataGenerator((n/10),d,p)
  x <- rbind(Datalist1[[1]],Datalist2[[1]],Datalist3[[1]],Datalist4[[1]],
             Datalist5[[1]],Datalist6[[1]],Datalist7[[1]],
               Datalist8[[1]],Datalist9[[1]],Datalist10[[1]])
  y <- rbind(Datalist1[[2]],Datalist2[[2]],Datalist3[[2]],Datalist4[[2]],
             Datalist5[[2]],Datalist6[[2]],Datalist7[[2]],
               Datalist8[[2]],Datalist9[[2]],Datalist10[[2]])
  return(list(x,y)) 
}
 

generRegData.Prepackages <- c("MASS")


comment(generRegData.DataGenerator) <- c("")