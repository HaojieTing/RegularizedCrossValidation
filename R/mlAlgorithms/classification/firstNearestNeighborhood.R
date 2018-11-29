# Letter数据集的学习算法之一。
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2018/2/3

firstNearestNeighborhood.TrainAndTest <- function(data_train, data_test, algorConf) {
  warning("This classifier is only suitable for letter dataset. So be careful if you wanna use it.")  
  xtrain <- as.matrix(data_train[, 1:16])
  y <- data_train[,17]
  xtest <- as.matrix(data_test[,1:16])
  if(  16 != ncol(xtrain) ) {
    stop("Only 16 features are legal.")
  }  
  w <- algorConf$w
  c1<-c(1,3,9,16)
  c3<-c(5, 11,13)
  
  count_train <- nrow(xtrain)
  count_test  <- nrow(xtest)
  
  preY<-rep(1, count_test)
  
  for(i in 1:count_test){
    M<-t(t(xtrain) - xtest[i,])^2
    M[,c1]<- M[,c1] * w
    M[,c3]<-M[,c3] * w^(-1)
    M<-rowSums(M)    
    
    min<-M[1]
    for(j in 2:count_train){
      if(min > M[j]){
        min<-M[j]
        preY[i]<-j
      }
    }
  }
  testYY <- y[preY]  
  return(testYY)
}


firstNearestNeighborhood.Prepackages <- c()


firstNearestNeighborhood.validation <- function(algorConf) {
  if( is.null(algorConf$w) ) return(FALSE)
  return(TRUE)
}