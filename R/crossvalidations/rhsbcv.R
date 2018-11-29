# 正则化Repeated Half-sampling交叉验证方法
#
# 基于正交表构造，出现频次均衡且重叠个数均衡的RRHS交叉验证
# 的切分集合
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/5/9
# 修改记录:
#    wrb@2017/11/9 修正了block-regularized repeated half-sampling方法。

rhsbcv.Generator <- function(cvConf) {
  if (is.null(cvConf$data)) stop("Please specify cv$data.")
  n <- nrow(cvConf$data)
  n1 <- cvConf$n1
  J <- cvConf$J
  n2 <- n - n1
  partitions <- vector("list", J)  
  if( (n%%2 == 0  && n1 == n/2) || (n%%2 == 1 && n1 == round(n/2)) || (n%%2 ==1 && n1 == round(n/2)+1)) {
    l <- NULL
    if (J%%2 == 1) {
      l <- (J+1)/2
    }  else {  
      l <- J/2
    }
    plan <- oa.design(nruns = 4*l, nfactors = 4*l-1, nlevels=2)
    oa<-matrix(t(as.numeric(as.matrix(plan))),4*l)
    orthArray<-data.frame((oa-1.5)*2)
    elimRow <- which(abs(rowSums(orthArray))==4*l-1)
    if(length(elimRow) == 1) {
      OA1 <- orthArray[-elimRow,]
      evenFactor <- sum(OA1[1,])
      OA2 <- OA1[, which(OA1[1,]==evenFactor)]
      OA3 <- OA2[-1,]    
      blocks <- standardVFCV(n, 1, nrow(OA3))
      for(j in 1:J) {
        split <- vector("list",2)
        blockIdx <- which(OA3[,j] == 1)
        for(k in blockIdx) {
          split[[1]] <- c(split[[1]], blocks[[k]])
        }
        split[[2]] <- (1:n)[-split[[1]]]
        partitions[[j]] <-split
      }
      rhsbcv <- list()
      rhsbcv[["partitions"]] <- partitions
      return(rhsbcv)
    }
    stop("Cannot construct corresponding balanced rhs cross-validaiton")
  } 
  stop("Cannot construct corresponding balanced rhs cross-validaiton")
}

rhsbcv.Prepackages <- c("DoE.base")


rhsbcv.validation <- function(cvConf) {
  valid <- TRUE
  if(is.null(cvConf$n1)) {
    warning("not provide n1")
    valid <- FALSE
  }
  if(is.null(cvConf$J)) {
    warning("not provide J")
    valid <- FALSE
  }
  return(valid)
}


standardVFCV <- function(n, n1, v, ...) {
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