# 随机mxk交叉验证的增量式版本。
#
# Auhtor: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/11/18

mxkrcv_inc.Generator <- function(cvConf) {
  n <- nrow(cvConf$data)  
  m <- cvConf$m  
  v <- cvConf$v
  m_prev <- cvConf$m_prev 
  if(is.null(m_prev)) 
    m_prev <- 0 
  if(is.null(cvConf$splits)){
    cvConf$splits <- list()
  }
  cvConf$splits_new <- list()
  for(temp_m in (m_prev+1):m){
    one_cv_splits <-standardVFCV(n, v)    
    for(i in 1:v) {      
      train_index <- c(1:n)[-one_cv_splits[[i]]]
      test_index <- one_cv_splits[[i]] 
      cvConf$splits[[length(cvConf$splits)+1]] <- list(train_index, test_index)
      cvConf$splits_new[[length(cvConf$splits_new)+1]] <- list(train_index, test_index)
    }
  }
  cvConf$m_prev <- m
  return(cvConf)
}

increaseBalancedMx2cv.Prepackages <- c()

increaseBalancedMx2cv.validation <- function(){
  if( is.null(cvConf$m)) return(FALSE)
  if( is.null(cvConf$v)) return(FALSE)
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
