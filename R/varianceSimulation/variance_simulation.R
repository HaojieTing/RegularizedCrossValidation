# 方差模拟的主程序模块
# 
# 给定数据集、算法以及交叉验证，获取
# 该数据在该算法下，所得的交叉验证估计的真实方差（重复次数较大）
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/5/21

library(snow)

repetition.total.count = varsim.conf.dataRepCount * varsim.conf.partitionSetRepCount

# 加载数据集的生成函数和附属包
dataset.info <- LoadDataSetGenerator(dataset.conf) 
dataset.generator <- dataset.info[[1]]
dataset.packages <- dataset.info[[2]]

# 加载机器学习算法的生成函数和附属包
algorithm.info <- LoadAlgorithmGenerator(algorithm.conf)
algorithm.generator <- algorithm.info[[1]]
algorithm.packages <- algorithm.info[[2]]

# 加载交叉验证方法的生成函数和附属包
crossvalidation.info <- LoadCrossValidationGenerator(crossvalidation.conf$name)
crossvalidation.generator <- crossvalidation.info[[1]]
crossvalidation.packages <- crossvalidation.info[[2]]

# 定义每一个计算节点上运行的子程序
slaver.function <- function(repetition.serial.no){
  # 在每个计算节点上产生单个数据集，
  # 并计算交叉验证切分集。进而计算
  # 数据集上的交叉验证估计。
  #
  # Args:
  #   repetition.serial.no: 重复次数的序号，即，对应第几次重复。
  dataset.index <- ((repetition.serial.no - 1) %% repetition.total.count) + 1  # 对应的是第几组数据集
  partitionset.index <- (repetition.total.count - 1) %/% repetition.total.count   # 对应的是第几组切分集
  # 产生数据集
  set.seed(dataset.index)  # 数据集产生的种子，通过设置为dataset.index, 可保证产生相同的数据集.
  dataset <- dataset.generator(dataset.conf)
  crossvalidation.conf$data <- dataset #付给交叉验证配置.
  dataset.size <- nrow(data)
  # 产生切分集
  if(varsim.conf.partitionSetRndSeed){
    set.seed(partitionset.index + 1111111)  # 其中， 1111111是固定加入的一个常数，无特定意义。
  }
  cv.object <- crossvalidation.generator(crossvalidation.conf)
  partitionset <- cv.object$partitions
  partitionset.size <- length(partitionset)
  # 训练并测试，得到交叉验证估计
  crossvalidation.estimator.results <- TrainAndTestForCrossValidationEstimatorWithPartitionSet(dataset, partitionset, 
                                                                   algorithm.generator, algorithm.conf)
  holdout.estimators <- crossvalidation.estimator.results[[2]]  # 取出所有holdout估计.
  crossvalidation.estimator <- crossvalidation.estimator.results[[1]]  # 取出交叉验证估计.
  # 定义估计向量，其中最后一列存交叉验证估计，余下存holdout估计
  estimator.vector <- rep(NA, length(partitionset.size) + 1)  
  estimator.vector[1: partitionset.size] <- holdout.estimators  # 存储holdout估计 
  estimator.vector[partitionset.size + 1] <- crossvalidation.estimator  # 存储交叉验证估计.
  estimator.vector
}

# 获取当前环境中的所有变量和函数
all.functionss.and.variables <- ls()
# 启动集群，并得到集群句柄
cluster.handler <- makeCluster() 
# 在集群中的所有节点上加载数据集、算法以及交叉验证所需的类库
ignore <- clusterCall(cluster.handler, WorkerInit, dataset.packages)
ignore <- clusterCall(cluster.handler, WorkerInit, algorithm.packages)
ignore <- clusterCall(cluster.handler, WorkerInit, crossvalidation.packages)
# 将当前环境中的所有变量和函数导入到集群的所有计算节点上
ignore<-clusterExport(cluster.handler, all.functionss.and.variables)
# 计算节点执行子程序
estimator.all.vector <- clusterApplyLB(cluster.handler, 1:repetition.total.count, slaver.function)
# 停止集群
stopCluster(cluster.handler)  

# 开始整理结果，并计算真实方差
estimator.matrix <- do.call(rbind, estimator.all.vector)
# 将矩阵中的所有NA缺失值，置为0.
estimator.matrix[is.na(estimator.matrix) ] <- 0.0
# 取出交叉验证估计值
crossvalidation.estimator.vector <- estimator.matrix[, ncol(estimator.matrix)]

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
