# 基于增量构造的mx2块正则化交叉验证.
# 
# Author: Wang ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: xxxx/xx/xx
# TODO List:
#   1. 增加分层交叉验证类型.
# 修改记录:
# 

mx2bcv_inc.Generator <- function(cvConf) {
  f2 <- function(x,y){return(y[x])}
  f3 <- function(x,y){return(y[[x]])}
  n <- NULL
  if (!is.null(cvConf$data)) { 
    n <- nrow(cvConf$data)  
  } else {
    n <- cvConf$n
  }
  m <- cvConf$m  
  if(m >= 2^floor(log(n,base=2))){
    stop("m is out of range.");
  }
  m_prev <- cvConf$m_prev 
  if(is.null(m_prev)) 
      m_prev <- 0 
  if(m_prev > 0) {
    if( is.null(cvConf$orthArray) ){
      stop("no orth array infor.")
    }
  }
  if( is.null(cvConf$orthArray) ) {
    cvConf$orthArray <- list()
  }
  if(is.null(cvConf$partitions)){
    cvConf$partitions <- list()
  }
  cvConf$splits_new <- list()
  for(temp_m in (m_prev+1):m){
    twoPowNum <- 2^floor(log(temp_m,base=2))
    if(temp_m == twoPowNum) {
      column <- rep(c(0,1), temp_m)
      cvConf$orthArray[[temp_m]] <- column      
    } else {
      diff <- temp_m - twoPowNum
      column1 <- cvConf$orthArray[[diff]]
      column2 <- cvConf$orthArray[[twoPowNum]]
      multiplier <- length(column2) / length(column1)
      column <- as.numeric(xor(rep(column1, each=multiplier), column2))
      cvConf$orthArray[[temp_m]] <- column     
    }
    #blocks
    if(is.null(cvConf$blocks)) {
      index.vec <- 1:n
      cvConf$blocks <- standardVFCV(index.vec,  2^(floor(log(temp_m,base=2))+1), cvConf)
    } else {
      multiplier <- 2^(floor(log(temp_m,base=2))+1) / length(cvConf$blocks)
      if(multiplier > 1) {
        blocks <- list()
        for(bindex in 1:length(cvConf$blocks)) {
          block_index <- standardVFCV(cvConf$blocks[[bindex]], multiplier, cvConf)
          blocks <- append(blocks, block_index)
        }
        cvConf$blocks <- blocks
      }
    }  
    #splits
    temp_split <- list()
    temp_dual_split <- list()
    nblocks <- 2^(floor(log(temp_m,base=2))+1)    
    column <- cvConf$orthArray[[temp_m]]    
    multiplier <- nblocks /length(column)
    newColumn <- rep(column, each=multiplier)
    
    zeroIndice <- which(newColumn == 0)    
    temp_split[[1]] <- unlist(lapply(zeroIndice, f3, cvConf$blocks))
    temp_split[[2]] <- (1:n)[-temp_split[[1]]]
    temp_dual_split[[1]] <- temp_split[[2]]
    temp_dual_split[[2]] <- temp_split[[1]]
    cvConf$partitions[[length(cvConf$partitions)+1]] <- temp_split
    cvConf$partitions[[length(cvConf$partitions)+1]] <- temp_dual_split
    cvConf$splits_new[[length(cvConf$splits_new)+1]] <- temp_split
    cvConf$splits_new[[length(cvConf$splits_new)+1]] <- temp_dual_split
  }
  cvConf$m_prev <- m
  return(cvConf)
}


mx2bcv_inc.Prepackages <- c()


mx2bcv_inc.validation <- function(cvConf) {
  if(is.null(cvConf$m)) {
    return(FALSE)
  }  
  
  return(TRUE)
}



standardVFCV <- function(index.vec, v, conf) {
  n <- length(index.vec)
  ngroup <-v  
  ngroup <- trunc(ngroup)
  if( ngroup < 2){
    stop ("folds < 2")
  }
  if(ngroup > n){
    stop ("folds > n")
  }
  if(ngroup<=n){
    o <- NULL
    if(is.null(conf$y.reg)) {
      o <- sample(index.vec)  
    } else {
      if(is.null(conf$data)) stop("please provide data")
      y <- conf$data[index.vec, ncol(conf$data)]
      index.reorder <- order(y,decreasing = T)
      o <- index.vec[index.reorder]
    }
    groups <- vector("list",ngroup)
    temp.index <- 1
    for(j in o){
      jj <- (temp.index-1) %% ngroup + 1
      groups[[jj]] <- c(groups[[jj]], j)
      temp.index <- temp.index+1
    }
  }
  return(groups)  
}


