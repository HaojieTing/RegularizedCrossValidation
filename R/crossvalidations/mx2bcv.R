# 块正则化的mx2交叉验证方法。
#
# 直接基于正交表来构造块正则化的mx2交叉验证切分集合。
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: XXXX/XX/XX
# 修改记录:
#   wrb@2017/11/8: 规范切分集合的取法，基于partitions取出切分集合.
#                  交叉验证全为从数据开始取出维度，摒弃n参数。

mx2bcv.Generator <- function(cvConf){
  if(is.null(cvConf$data)) stop("Please specify cv$data.")
  n <- nrow(cvConf$data)
  J <- cvConf$m 
  if (J >= 143 )  stop("the repitition count is out of range.")  
  if ( n< J + 1 ) stop("repitition count cannot larger than n-1")
  nruns = J+(4-J%%4)
  plan <- oa.design(nruns = nruns, nfactors = nruns-1, nlevels = 2)
  oa<-matrix(t(as.numeric(as.matrix(plan))),nruns)
  orthArray <- data.frame((oa-1.5)*2)
  orthArray <- orthArray[, 1:J]
  blocks<- standardVFCV(n, 1, nrow(orthArray[1]))
  mgroupsCVs<-vector("list",J)
  for(i in 1:J){
    split<-orthArray[i]       
    cvs<-vector("list",2)
    for(b in 1:nrow(split)){      
      if(split[b,1]==1){        
        if(is.null(cvs[[1]])){
          cvs[[1]]<-blocks[[b]]
        }else{
          cvs[[1]]<-cbind(t(as.numeric(cvs[[1]])),t(as.numeric(blocks[[b]])))
        }
      }else{
        if(is.null(cvs[[2]])){
          cvs[[2]]<-blocks[[b]]
        }else{
          cvs[[2]]<-cbind(t(as.numeric(cvs[[2]])),t(as.numeric(blocks[[b]])))
        }
      }
    }
    cvs[[1]]<-sort(cvs[[1]])
    cvs[[2]]<-sort(cvs[[2]])
    mgroupsCVs[[i]]<-cvs
  }
  mx2bcv <- list()
  mx2bcv[["partitions"]] <- vector("list", J * 2)
  for(j in 1:J) {
    cvsplit <- mgroupsCVs[[j]]
    mx2bcv[["partitions"]][[2*(j-1)+1]] <- list(cvsplit[[1]], cvsplit[[2]])
    mx2bcv[["partitions"]][[2*(j-1)+2]] <- list(cvsplit[[2]], cvsplit[[1]])
  }
  return(mx2bcv)
}

mx2bcv.Prepackages <- c("DoE.base")

mx2bcv.validation <- function(cvConf) {
  if(is.null(cvConf$m) ) {
    return(FALSE)
  }
  if(is.null(cvConf$n)) {
    warning("n is not given.")
  }
    
  return(TRUE)
}


standardVFCV <- function(n, n1, v, ...) {
  ngroup <-v  
  ngroup <- trunc(ngroup)
  if( ngroup < 2){
    stop ("folds count is less than 2.")
  }
  if(ngroup > n){
    stop ("folds count is larger than n.")
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