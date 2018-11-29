groupsnumber<-function(n){
  m <- n/10
  k <- m/10
  a <- c(1:n)
  xx<-((a-1)%% m) %/% k+1
  return(xx)
}


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
  epsilons <- matrix(mvrnorm(1,emean,evar),nn,1)
  y <- x%*%coef+ epsilons
  return(list(x,y))
}


corRegDataNB_nooutliers_rowblock.DataGenerator<-function(n, d, p=0.1){
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
  xx <- rbind(Datalist1[[1]],Datalist2[[1]],Datalist3[[1]],Datalist4[[1]],
             Datalist5[[1]],Datalist6[[1]],Datalist7[[1]],
             Datalist8[[1]],Datalist9[[1]],Datalist10[[1]])
  yy <- rbind(Datalist1[[2]],Datalist2[[2]],Datalist3[[2]],Datalist4[[2]],
             Datalist5[[2]],Datalist6[[2]],Datalist7[[2]],
             Datalist8[[2]],Datalist9[[2]],Datalist10[[2]])
  idx<-groupsnumber(n)
  data_x_new <- cbind(xx,idx)
  data_y_new <- cbind(yy,idx)
  x <- (data_x_new[order(data_x_new[,ncol(data_x_new)]),])
  x <- x[, -ncol(x)]
  y <- (data_y_new[order(data_y_new[,ncol(data_y_new)]),])
  y <- y[, -ncol(y)]
  return(list(x,y))
}



  
    
