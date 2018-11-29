# repeated learning-testing 交叉验证算法
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: xxxx/xx/xx
# 修改记录:
#   wrb@2017/11/9: 修正了rlt切分集的存储方式。


rlt.Generator <- function(cvConf) {
  if (is.null(cvConf$data)) stop("Please specify cv$data.")
  n <- nrow(cvConf$data)
  n1 <- cvConf$n1
  J <- cvConf$J
  if(n1 >= n)
    stop("n1 must be less than n")
  partitions<-vector("list", J)
  for(i in 1:J){
    o<-sample(1:n)
    split<-vector("list", 2)
    split[[1]]<-o[1:n1]
    split[[2]]<-o[(n1+1):n]
    partitions[[i]]<-split
  }    
  rlt <- list(
    "partitions" = partitions
  )
  return(rlt)
}


rlt.Prepackages <- c()


rlt.validation <- function(cvConf) {
  if(is.null(cvConf$n1)) return(FALSE)
  if(is.null(cvConf$J)) return(FALSE)
  return(TRUE)
}