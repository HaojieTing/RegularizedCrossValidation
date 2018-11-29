# 标准K-折交叉验证
#
# Author: wang ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: xxxx/xx/xx
# 修改记录:
#   wrb@2017/11/9: 修正了标准K折交叉验证的切分存储方式。

standardVFCV.Generator <- function(cvConf) {
  if (is.null(cvConf$data)) stop("Please specify cv$data.")
  n <- nrow(cvConf$data)
  v <- cvConf$v
  ngroup <- v  
  ngroup <- trunc(ngroup)
  if( ngroup < 2)   stop ("nfolds < 2")
  if(ngroup > n)    stop ("folds > n")
  if(ngroup<=n){
    o <- sample(1:n)
    groups <- vector("list",ngroup)
    for(j in 1:n){
      jj <- (j-1) %% ngroup + 1
      cnt <- trunc((j-1) / ngroup)
      groups[[jj]][cnt + 1] <- o[j]
    }
  }
  partitions <- vector("list", v)
  for(gIdx in 1:v) {
    one_holdout <- vector("list", 2)
    one_holdout[[2]]  <- sort(groups[[gIdx]])
    one_holdout[[1]] <- (1:n)[-groups[[gIdx]]]
    partitions[[gIdx]] <- one_holdout
  }
  standardVFCV <- list(
     "partitions" = partitions
  )
  return(standardVFCV)
}


standardVFCV.Prepackages <- c()

standardVFCV.validation <- function(cvConf) {
  if(is.null(cvConf$v)) return(FALSE)
  return(TRUE)
}