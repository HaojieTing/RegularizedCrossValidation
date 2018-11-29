corMatrixGenerator<-function(n,k=10,p){
  zeromatrix<-matrix(rep(0,(n/k)^2),n/k)
  diagcormatrix<-matrix(rep(p,(n/k)^2),n/k)
  cormatrix<-matrix(rep(NA,n^2),n)
  cormatrix[1:(n/k),]<-cbind(diagcormatrix,matrix(rep(0,(n/k)*(k-1)/k*n),n/k))
  cormatrix[((n/k)+1):(2*n/k),]<-cbind(zeromatrix,diagcormatrix,matrix(rep(0,(n/k)*(k-2)/k*n),n/k))
  cormatrix[((2*n/k)+1):(3*n/k),]<-cbind(zeromatrix,zeromatrix,diagcormatrix,matrix(rep(0,(n/k)*(k-3)/k*n),n/k))
  cormatrix[((3*n/k)+1):(4*n/k),]<-cbind(zeromatrix,zeromatrix,zeromatrix,diagcormatrix,matrix(rep(0,(n/k)*(k-4)/k*n),n/k))
  cormatrix[((4*n/k)+1):(5*n/k),]<-cbind(zeromatrix,zeromatrix,zeromatrix,zeromatrix,diagcormatrix,
                                         matrix(rep(0,(n/k)*(k-5)/k*n),n/k))
  cormatrix[((5*n/k)+1):(6*n/k),]<-cbind(zeromatrix,zeromatrix,zeromatrix,zeromatrix,zeromatrix,diagcormatrix,
                                         matrix(rep(0,(n/k)*(k-6)/k*n),n/k))
  cormatrix[((6*n/k)+1):(7*n/k),]<-cbind(zeromatrix,zeromatrix,zeromatrix,zeromatrix,zeromatrix,zeromatrix,diagcormatrix,
                                         matrix(rep(0,(n/k)*(k-7)/k*n),n/k))
  cormatrix[((7*n/k)+1):(8*n/k),]<-cbind(zeromatrix,zeromatrix,zeromatrix,zeromatrix,zeromatrix,zeromatrix,zeromatrix,diagcormatrix,
                                         matrix(rep(0,(n/k)*(k-8)/k*n),n/k))
  cormatrix[((8*n/k)+1):(9*n/k),]<-cbind(zeromatrix,zeromatrix,zeromatrix,zeromatrix,zeromatrix,zeromatrix,zeromatrix,zeromatrix,
                                         diagcormatrix,matrix(rep(0,(n/k)*(k-9)/k*n),n/k))
  cormatrix[((9*n/k)+1):n,]<-cbind(zeromatrix,zeromatrix,zeromatrix,zeromatrix,zeromatrix,zeromatrix,zeromatrix,zeromatrix,zeromatrix,
                                   diagcormatrix)
  return(cormatrix)
}


groupsnumber<-function(n){
  m <- n/10
  k <- m/10
  a <- c(1:n)
  xx<-((a-1)%% m) %/% k+1
  return(xx)
}


two_normals_classificationDataGenerator <- function(n0,n1,mu0,mu1,sigma0,sigma1){
  require(MASS)
  x0 <- mvrnorm(n0,mu0,sigma0)
  x1 <- mvrnorm(n1,mu1,sigma1)
  x <- rbind(x0,x1)
  y <- c(rep(0,n0),rep(1,n1))
  list.xy <- cbind(x,y)
  return(list.xy)
}


cor_two_normals_classification_indexDataGenerator<- function(n,k=10,p){
  cormatrix <-corMatrixGenerator(n,k=10,p)
  number<-matrix(NA,k,n/k)
  for(i in 1:k){
    number[i,1] <-sample(((i-1)*n/k+1):(i*n/k),1)
    o<-number[i,1] 
    for(j in 2:(n/k)){
      number[i,j]<-sample((1:n),1,prob=cormatrix[o,],replace=F)
      o <- number[i,j]
    }
  }
  return(number)  
}


cor_two_normals_classification_rowblock.DataGenerator <- function(dataConf){
  require(MASS)
  n <- dataConf$n
  n0 <- dataConf$n0
  n1 <- dataConf$n1
  mu0 <- dataConf$mu0
  mu1 <- dataConf$mu1
  sigma0 <- dataConf$sigma0
  sigma1 <- dataConf$sigma1
  k <- dataConf$k
  p <- dataConf$p
  data <- two_normals_classificationDataGenerator(n0,n1,mu0,mu1,sigma0,sigma1)
  index <- cor_two_normals_classification_indexDataGenerator(n,k=10,p)
  new_data <-matrix(NA,n,ncol(data))
  for(i in 1:k){
    new_data[((i-1)*n/k+1):(i*n/k),] <- data[index[i,],]
  }
  xx <- new_data[, -ncol(new_data)]
  yy <- new_data[, ncol(new_data)]
  idx<-groupsnumber(n)
  data_x_new <- cbind(xx,idx)
  data_y_new <- cbind(yy,idx)
  x <- (data_x_new[order(data_x_new[,ncol(data_x_new)]),])
  x <- x[, -ncol(x)]
  y <- (data_y_new[order(data_y_new[,ncol(data_y_new)]),])
  y <- y[, -ncol(y)]
  return(list(x,y))
  
}


cor_two_normals_classification_rowblock.Prepackages <- c("MASS")


cor_two_normals_classification_rowblock.validation <- function(dataConf) {
  if(is.null(dataConf$n))    {  return(FALSE)  }
  if(is.null(dataConf$n0))    {  return(FALSE)  }
  if(is.null(dataConf$n1))    { return(FALSE) }
  if(is.null(dataConf$mu0))   { return(FALSE)}
  if(is.null(dataConf$mu1))   { return(FALSE) }
  if(is.null(dataConf$sigma0)) {return(FALSE)}
  if(is.null(dataConf$sigma1)) {return(FALSE)}
  if(is.null(dataConf$k))    {  return(FALSE)  }
  if(is.null(dataConf$p))    {  return(FALSE)  }
  return(TRUE)
}
