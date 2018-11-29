# 协方差函数模拟的主程序模块。
#
# 通过协方差函数模拟，可以获得在不同的重叠个数下，协方差值以及
# 协方差函数的参数值（\sigma^2, \gamma, \tau, \omega）.
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/5/9
#
# TODO(2017/9/12): 加入不同的评价指标类型。

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

# 使用并行方式计算协方差图像
#加载并行计算
library(doSNOW)
library(foreach)
workdir.path <- getwd()  
all.funs.and.variables <- ls()
# 启动集群
cluster.handler <- makeCluster()
# 注册集群并将临时变量导入到每个节点上。
registerDoSNOW(cluster.handler)
export.ignore <- clusterCall(cluster.handler, WorkerInit, dataset.packages)
export.ignore <- clusterCall(cluster.handler, WorkerInit, algorithm.packages)
export.ignore <- clusterCall(cluster.handler, WorkerInit, crossvalidation.packages)
export.ignore <- clusterExport(cluster.handler, all.funs.and.variables)
# 每个节点开始训练并测试模型，计算损失和期望预测误差
repetition.results <- foreach(r=1:task.conf.rpt) %dopar% {
  # 设置随机数种子，保证实验结果可重复
  set.seed(r) 
  dataset.observants <- dataset.generator(dataset.conf) 
  # 将数据集压入到交叉验证配置中，有助于分层交叉验证实施
  crossvalidation.conf$data <- dataset.observants
  # 为了保证所产生的数据切分相同，需要设置相同的随机数种子
  # TODO(wangruibo@2017/05/10) 测试这里的常数随机数种子在不同的机器上都可以产生相同的切分。
  set.seed(123) 
  cv.object.first <- crossvalidation.generator(crossvalidation.conf)
  partition.first <- cv.object.first$partitions
  dataset.size <- nrow(dataset.observants)  # 数据集大小 
  dataset.trainingsize <- task.conf.n1  # 训练集大小
  if (dataset.trainingsize >= dataset.size) {
    stop("Training set should not large than entire data set size!")
  }
  count.overlap <- task.conf.overlapCount  # 训练集重叠个数
  count.exchange <- dataset.trainingsize - count.overlap  # 训练集交换个数
  if (count.exchange > min(dataset.trainingsize, dataset.size - dataset.trainingsize)) {
    stop("Count of exchanged  observants is out of its feasible scope!")
  }
  # 根据第一个切分和交换的观测个数，计算出第二个切分。
  # 
  # 计算第二个切分时，可以采用随机和固定两种方式。
  # 在协方差模拟时，我们采用固定交换的方式产生第二种切分。
  # 随机方式应该使用randomExchangeingIndexes函数，这里我们不提供支持。
  cv.object.second <- SwapObservantsToConstructNewPartition(partition.first, count.exchange)
  partition.second <- cv.object.second$partitions
  pe.holdouts <- rep(NA, 2) #存储两组切分对应的4个holdout的预测误差估计
  # 计算Hold-out估计
  pe.first  <- TrainAndTestForHoldoutEstimator(dataset.observants, partition.first, algorithm.generator, algorithm.conf)    
  pe.second <- TrainAndTestForHoldoutEstimator(dataset.observants, partition.second, algorithm.generator, algorithm.conf)    
  # 存储Hold-out估计、及相应的损失值
  pe.holdouts[1] <- pe.first[[1]]
  pe.holdouts[2] <- pe.second[[1]]
  loss.matrix <- matrix(rep(NA, 2*(dataset.size - dataset.trainingsize)), 2)
  loss.matrix[1,] <- pe.first[[2]]
  loss.matrix[2,] <- pe.second[[2]]
  if(!exists("debug_info")) {
    list(pe.holdouts, loss.matrix)
  } else {
    list(pe.holdouts, loss.matrix, debug_info)
  }
}  
# 停止集群
stopCluster(cluster.handler)

# 预处理多次模拟的结果
pe.holdout.matrix <- do.call(cbind,lapply(repetition.results, "[[", 1))  # 行为重复次数，列为2.
loss.matrix.first <- do.call(rbind,lapply(repetition.results, function(mat){ return(mat[[2]][1,])}))
loss.matrix.second <- do.call(rbind,lapply(repetition.results, function(mat){ return(mat[[2]][2,])}))
# 此时说明有debug信息，将其直接写出，以待后续分析。
save(repetition.results, file = paste(data.dir, "repetition.result", sep=.Platform$file.sep))


# 计算协方差值
count.swap <- task.conf.n1 - task.conf.overlapCount
covariance.value <- cov(pe.holdout.matrix[1,], pe.holdout.matrix[2,])  #协方差值
variance.value.first <- var(pe.holdout.matrix[1,])
variance.value.second <- var(pe.holdout.matrix[2,])
variance.value <- sqrt(variance.value.first) * sqrt(variance.value.second)   # 方差值
correlation.value <- cor(c(pe.holdout.matrix[1,]), c(pe.holdout.matrix[2,]))  # 相关系数  

# 计算协方差值，及协方差函数的参数值及系数值
# 基于损失函数值，计算协方差矩阵
covariance.matrix <- cov(x = loss.matrix.first, y = loss.matrix.second)
# 按重叠个数，分情形讨论各个参数取值
parameter.sigma2 <- NA
parameter.omega  <- NA
parameter.tau    <- NA
parameter.gamma  <- NA
coeff.x2 <- NA
coeff.x1 <- NA
coeff.x0 <- NA
overlap.count.min <- max(0, 2*task.conf.n1 - task.conf.n)
validation.size <- task.conf.n - task.conf.n1  # 验证集大小
if (task.conf.overlapCount == overlap.count.min) {
  # 当重叠个数为0时，只有gamma存在.
  parameter.gamma <- mean(covariance.matrix)
  # 当重叠个数为0时，只有常数项存在.
  coeff.x0 <- sum(covariance.matrix)
} else if (task.conf.overlapCount == overlap.count.min + 1) {
  # 当重叠个数为1是，只有sigma^2, tau和gamma存在
  covariance.part.first <- covariance.matrix[1:(validation.size - 1), 1:(validation.size - 1)]
  covariance.part.second <- covariance.matrix[1:(validation.size - 1), validation.size]
  covariance.part.third <- covariance.matrix[validation.size, 1:(validation.size - 1)]
  covariance.part.fourth <- covariance.matrix[validation.size, validation.size]
  # 计算协方差的各个参数值
  parameter.sigma2 <- covariance.part.fourth
  parameter.tau <- (mean(covariance.part.second) + mean(covariance.part.third)) /2
  parameter.gamma <- mean(covariance.part.first)
  # 当重叠个数为1时，只有常数项存在
  coeff.x0 <- sum(covariance.matrix)
} else if (task.conf.overlapCount == task.conf.n1 -1) {
  # 当重叠个数为训练集大小-1时，gamma成为一个数值, tau称为两个向量.
  covariance.part.first <- covariance.matrix[1, 1]
  covariance.part.second <- covariance.matrix[1, 2:validation.size]
  covariance.part.third <- covariance.matrix[2:validation.size, 1]
  covariance.part.fourth <- covariance.matrix[2:validation.size, 2:validation.size]
  # 计算协方差的各个参数值
  covariance.diagonal <- diag(covariance.part.fourth)
  parameter.sigma2 <- mean(covariance.diagonal)
  diag(covariance.part.fourth) <- NA
  parameter.omega <- mean(covariance.part.fourth, na.rm = TRUE)
  parameter.tau <- (mean(covariance.part.second) + mean(covariance.part.third)) /2
  parameter.gamma <- covariance.part.first
  # 计算函数系数
  coeff.x2 <- parameter.omega + parameter.gamma - 2*parameter.tau
  coeff.x1 <- parameter.sigma2 + (2*(task.conf.n - 2* task.conf.n1) - 1)*parameter.omega - 2* task.conf.n1 * parameter.gamma - 2* (3*task.conf.n1 - task.conf.n)*parameter.tau
  coeff.x0 <- (task.conf.n - 2* task.conf.n1)*parameter.sigma2 + (task.conf.n - 2* task.conf.n1)*(task.conf.n - 2* task.conf.n1-1)*parameter.omega + task.conf.n1^2*parameter.gamma + 2* task.conf.n1*(task.conf.n - 2* task.conf.n1)*parameter.tau
} else if (task.conf.overlapCount == task.conf.n1) {
  # 当重叠个数为训练集大小时，gamma和tau不能存在，只有sigma^2和omega
  covariance.part.first <- covariance.matrix[1:validation.size, 1:validation.size]
  covariance.diagonal <- diag(covariance.part.first)
  # 计算协方差的各个参数值
  parameter.sigma2 <- mean(covariance.diagonal)
  diag(covariance.part.first) <- NA
  parameter.omega <- mean(covariance.part.first, na.rm = TRUE)
  coeff.x0 <- sum(covariance.matrix)
} else {
  swap.count <- task.conf.n1 - task.conf.overlapCount
  # 将协方差矩阵分块
  covariance.matrix.first  <- covariance.matrix[1:swap.count, 1:swap.count]
  covariance.matrix.second <- covariance.matrix[1:swap.count, (swap.count+1):validation.size]
  covariance.matrix.third  <- covariance.matrix[(swap.count+1):validation.size, 1:swap.count] 
  covariance.matrix.fourth <- covariance.matrix[(swap.count+1):validation.size, (swap.count+1):validation.size]
  covariance.diagonal <- diag(covariance.matrix.fourth)
  # 计算协方差函数的各个参数值
  parameter.sigma2 <- mean(covariance.diagonal)
  diag(covariance.matrix.fourth) <- NA
  parameter.omega <- mean(covariance.matrix.fourth, na.rm = TRUE)
  parameter.tau <- (mean(covariance.matrix.second) + mean(covariance.matrix.third)) /2
  parameter.gamma <- mean(covariance.matrix.first)
  # 计算函数系数
  coeff.x2 <- parameter.omega + parameter.gamma - 2*parameter.tau
  coeff.x1 <- parameter.sigma2 + (2*(task.conf.n - 2* task.conf.n1) - 1)*parameter.omega - 2* task.conf.n1 * parameter.gamma - 2* (3*task.conf.n1 - task.conf.n)*parameter.tau
  coeff.x0 <- (task.conf.n - 2* task.conf.n1)*parameter.sigma2 + (task.conf.n - 2* task.conf.n1)*(task.conf.n - 2* task.conf.n1-1)*parameter.omega + task.conf.n1^2*parameter.gamma + 2* task.conf.n1*(task.conf.n - 2* task.conf.n1)*parameter.tau
}