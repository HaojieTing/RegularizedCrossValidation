# 随机mxk交叉验证方法。
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: xxxx/xx/xx
# 修改记录:
#    wrb@2017/11/9: 修正了交叉验证的存取方式

mxkrcv.Generator <- function(cvConf) {
  if (is.null(cvConf$data)) stop("Please specify cv$data.")
  n <- nrow(cvConf$data)
  J <- cvConf$m 
  v <- cvConf$v
  partitions <- vector("list", J * v)
  r <- 1
  for(j in 1:J ){
    one_cv_splits <-standardVFCV(n, v)    
    for(i in 1:v) {      
      partitions[[r]] <- vector("list", 2)
      partitions[[r]][[1]] <- c(1:n)[-one_cv_splits[[i]]]
      partitions[[r]][[2]] <- one_cv_splits[[i]] 
      r = r+ 1
    }
  }
  holdout <- list()
  holdout[["partitions"]] <- partitions
  return(holdout)
}


mxkrcv.Prepackages <- c()


mxkrcv.validation <- function(cvConf) {
  if( is.null(cvConf$m)) return(FALSE)
  if( is.null(cvConf$v)) return(FALSE)
  return(TRUE)
}


standardVFCV <- function(n, v) {
  ngroup <-v  
  ngroup <- trunc(ngroup)
  if( ngroup < 2){
    stop ("folds < 2")
  }
  if(ngroup > n){
    stop ("folds > n")
  }
  if(ngroup<=n){
    o <- sample(1:n)
    groups <- vector("list",ngroup)
    for(j in 1:n){
      jj <- (j-1) %% ngroup + 1
      cnt <- trunc((j-1) / ngroup)
      groups[[jj]][cnt + 1] <- o[j]
    }
  }
  for(gIdx in 1:v)
    groups[[gIdx]]=sort(groups[[gIdx]])
  return(groups)  
}