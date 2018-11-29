# 方差模拟实验中所用的常用函数汇总。
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/5/21

computeVarianceOfEstimators <- function(estimator.vector, partitionSetRepCount, dataRepCount) {
  # 根据估计值向量、数据切分次数以及数据重复次数，计算出估计的方差。
  #
  # Args:
  #   estimator.vector: 估计值向量
  #   partitionSetRepCount: 切分集合重复次数。
  #   dataRepCount: 数据重复次数
  # Returns:
  #  variance.total: 总体方差
  crossvalidation.estimator.vector <- estimator.vector
  varsim.conf.partitionSetRepCount <- partitionSetRepCount
  varsim.conf.dataRepCount <- dataRepCount
  # 要计算的总方差
  variance.total <- NULL
  #条件期望和条件方差分解
  estimator.vars.conditioned.on.partitionset <-  rep(NA, varsim.conf.partitionSetRepCount)
  estimator.mean.conditioned.on.partitionset <-  rep(NA, varsim.conf.partitionSetRepCount)
  
  mean.estimator.vars <- NA
  var.estimator.mean  <- NA
  
  if(varsim.conf.dataRepCount > 1) {
    for(i in 1:varsim.conf.partitionSetRepCount){  
      estimator.vars.conditioned.on.partitionset[i]  <- var(crossvalidation.estimator.vector[((i-1)*varsim.conf.dataRepCount+1):(i*varsim.conf.dataRepCount)]) 
      estimator.mean.conditioned.on.partitionset[i] <- mean(crossvalidation.estimator.vector[((i-1)*varsim.conf.dataRepCount+1):(i*varsim.conf.dataRepCount)])
    }
    if(varsim.conf.partitionSetRepCount == 1){
      mean.estimator.vars <- mean(estimator.vars.conditioned.on.partitionset)
      variance.total <- mean.estimator.vars
    } else {
      mean.estimator.vars <- mean(estimator.vars.conditioned.on.partitionset)
      var.estimator.mean  <- var(estimator.mean.conditioned.on.partitionset)
      variance.total <- mean.estimator.vars + var.estimator.mean
    }
  } else if(varsim.conf.dataRepCount == 1) {
    variance.total = var(crossvalidation.estimator.vector)
  }
  return(variance.total)
}

computeVarianceEstimatorOfmx2CV <- function(estimator.matrix, m.needed, m.total, split.count, data.count) {
  col.count <- ncol(estimator.matrix)
  row.count <- nrow(estimator.matrix)
  if(col.count != 2*m.total+1) {
    stop("col error")
  } 
  if(row.count != data.count* split.count) {
    stop("row error")
  }
  if(m.needed >= m.total) {
    stop("computation is not needed")
  }
  pertubs <- t(combn(m.total, m.needed))*2
  pertubs.row <- nrow(pertubs)
  if(pertubs.row >100) {
    pertubs.row <- 100
  }
  var.vector <- c()
  for(i in 1:pertubs.row) {
    a.row <- c(pertubs[i,])
    col.index <- c()
    for(j in a.row) {
      col.index <- c(col.index, j-1, j)
    }
    estimator.submatrix <- estimator.matrix[, col.index]
    estimator.vector <- rowMeans(estimator.submatrix)
    var.vector <- c(var.vector,computeVarianceOfEstimators(estimator.vector, split.count, data.count))
  }
  return (list(mean(var.vector),var(var.vector)))
}


computeVarianceOfRLT <- function(estimator.matrix, j.needed, j.total, split.count, data.count) {
  col.count <- ncol(estimator.matrix)
  row.count <- nrow(estimator.matrix)
  if(col.count != j.total+1) {
    stop("col error")
  }
  if(row.count != data.count* split.count) {
    stop("row error")
  }
  if(j.needed >= j.total) {
    stop("computation is not needed")
  }
  pertubs <- t(combn(j.total, j.needed))
  pertubs.row <- nrow(pertubs)
  if(pertubs.row >100) {
    pertubs.row <- 100
  }
  var.vector <- c()
  for(i in 1:pertubs.row) {
    a.row <- c(pertubs[i,])
    estimator.submatrix <- estimator.matrix[, a.row]
    estimator.vector <- rowMeans(estimator.submatrix)
    var.vector <- c(var.vector,computeVarianceOfEstimators(estimator.vector, split.count, data.count))
  }
  return (list(mean(var.vector),var(var.vector)))
}
