# 两个hold-out估计之间的协方差模拟。 
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2018/1/15

slaver.function <- function(r, task_config){
  if(!is.null(task_config$workdir)) setwd(task_config$workdir)
  dataConf  <- task_config$dataset.conf
  algorConf <- task_config$algorithm1.conf
  cvConf    <- task_config$crossvalidation.conf
  # 设置随机数种子，保证实验结果可重复
  DataInfo <- LoadDataSetGenerator(dataConf)
  DataGenerator <- DataInfo[[1]]
  DataPackages <- DataInfo[[2]]
  # 生成要检验的第一个算法
  AlgorInfo <- LoadAlgorithmGenerator(algorConf)
  AlgorGenerator <- AlgorInfo[[1]]
  AlgorPackages <- AlgorInfo[[2]]
  # 生成要使用的交叉验证
  CrossValidationInfo <- LoadCrossValidationGenerator(crossvalidation.name = cvConf$name)
  CrossValidationGenerator <- CrossValidationInfo[[1]]
  CrossValidationPackages <- CrossValidationInfo[[2]]
  WorkerInit(DataPackages)
  WorkerInit(AlgorPackages)
  WorkerInit(CrossValidationPackages)
  crossvalidation.conf <- task_config$crossvalidation.conf
  task.conf.n1 <- crossvalidation.conf$n1
  task.conf.n <- crossvalidation.conf$n
  task.conf.overlapCount <- task_config$overlap_count
  #开始生成相应的损失
  #set.seed(r) 
  dataset.conf <- task_config$dataset.conf
  dataset.observants <- DataGenerator(dataset.conf) 
  # 将数据集压入到交叉验证配置中，有助于分层交叉验证实施
  crossvalidation.conf$data <- dataset.observants
  # 为了保证所产生的数据切分相同，需要设置相同的随机数种子
  # TODO(wangruibo@2017/05/10) 测试这里的常数随机数种子在不同的机器上都可以产生相同的切分。
  # set.seed(123) 
  cv.object.first <- CrossValidationGenerator(crossvalidation.conf)
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
  partition.second <- SwapObservantsToConstructNewPartition(partition.first, count.exchange)
  # 计算Hold-out估计
  pe.first  <- TrainAndTestForHoldoutEstimator(dataset.observants, partition.first, AlgorGenerator, algorConf)    
  pe.second <- TrainAndTestForHoldoutEstimator(dataset.observants, partition.second, AlgorGenerator, algorConf)    
  values <- c( pe.first[[2]],  pe.second[[2]])
  # 生成要检验的第二个算法
  if(!is.null(task_config$algorithm2.conf)) {
    algorConf2<- task_config$algorithm2.conf
    algorConf2 <- ValidateAndResolveAlgorithmConfiguration(algorConf2)
    AlgorInfo2 <- LoadAlgorithmGenerator(algorConf2)
    AlgorGenerator2 <- AlgorInfo2[[1]]
    AlgorPackages2 <- AlgorInfo2[[2]]
    WorkerInit(AlgorPackages2)
    pe2.first  <- TrainAndTestForHoldoutEstimator(dataset.observants, partition.first, AlgorGenerator2, algorConf2)    
    pe2.second <- TrainAndTestForHoldoutEstimator(dataset.observants, partition.second, AlgorGenerator2, algorConf2)    
    values <- c(values, pe2.first[[2]], pe2.second[[2]])
  }
  values
}


covariance_simulation.compute_covariance_parameters <- function(loss.matrix.first, loss.matrix.second, task_config) {
  # 计算协方差值，及协方差函数的参数值及系数值
  # 基于损失函数值，计算协方差矩阵
  crossvalidation.conf <- task_config$crossvalidation.conf
  task.conf.n1 <- crossvalidation.conf$n1
  task.conf.n <- crossvalidation.conf$n
  task.conf.overlapCount <- task_config$overlap_count
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
  # 计算sigma^2和rho
  ho.est.first <- rowMeans(loss.matrix.first)
  ho.est.second <- rowMeans(loss.matrix.second)
  sigma2 <- (var(ho.est.first) + var(ho.est.second))/2
  rho <- cor(x = ho.est.first, y = ho.est.second)
  cova <- cov(x = ho.est.first, y = ho.est.second)
  parameters <- t(c(parameter.sigma2, parameter.omega, parameter.gamma,  parameter.tau, cova, sigma2, rho))
  colnames(parameters) <- c("sigma^2", "omega", "gamma","tau", "cova", "sigma2", "rho")
  return(parameters)
}

covariance_simulation.task_config_validation <- function(task_config) {
  # 验证协方差模拟任务的配置是否正确.
  #
  # Args:
  #   task_config: 任务配置信息
  # Returns:
  #   更新后的任务配置
  source("./utils.R", encoding = "UTF-8")
  source("./datasets/data_loader.R", encoding = "UTF-8")
  source("./crossvalidations/cv_loader.R", encoding="UTF-8")
  source("./mlAlgorithms/algor_loader.R", encoding="UTF-8")
  source("./model_train_and_predict.R", encoding = "UTF-8")
  # 重叠个数
  if (is.null(task_config$overlap_count)) stop("Need to specify 'overlapCount' for covariance simulation.") 
  # 重复次数
  if (is.null(task_config$rpt)) stop("Need to specify 'rpt' for covariance simulation.")
  # 是否顺序执行
  if (is.null(task_config$seq))        task_config[["seq"]] <- FALSE
  # 数据集配置
  if (is.null(task_config$dataset.conf))    stop("Please specify data set configuration.")
  task_config$dataset.conf <- ValidateAndResolveDataSetConfiguration(task_config$dataset.conf)
  # 算法1配置
  if (is.null(task_config$algorithm1.conf)) stop("Please specify the first algorithm configuration.")
  task_config$algorithm1.conf <- ValidateAndResolveAlgorithmConfiguration(task_config$algorithm1.conf)
  # 算法2配置
  if (!is.null(task_config$algorithm2.conf)) 
    task_config$algorithm2.conf <- ValidateAndResolveAlgorithmConfiguration(task_config$algorithm2.conf)
  # 交叉验证配置
  if (is.null(task_config$crossvalidation.conf)) stop("Please specify the cross validation configuration.")
  crossvalidation.conf <- task_config$crossvalidation.conf
  if( "hold_out" != crossvalidation.conf$name) stop("Covariance simulation only support hold-out validation")
  return(task_config)
}

cl <- NULL
cores <- NULL 

covariance_simulation.perform_task <- function(task_config) {
  crossvalidation.conf <- task_config$crossvalidation.conf
  task.conf.overlapCount <- task_config$overlap_count
  task.conf.rpt <- task_config$rpt
  crossvalidation.conf <- task_config$crossvalidation.conf
  task.conf.n1 <- crossvalidation.conf$n1
  task.conf.n <- crossvalidation.conf$n
  # 执行协方差模拟
  library(foreach)
  if (task_config[["seq"]]) {
  loss.estimator.matrix <- foreach(r=1:task.conf.rpt, .combine=rbind, .export = ls()) %do% 
      slaver.function(r, task_config)
  } else {
      # 并行处理该任务。
      require(foreach)
      require(doParallel)
      cluster.handler <- makeCluster(type="MPI")
      registerDoParallel(cluster.handler)
      work.directory <- getwd()
      loss.estimator.matrix <- foreach(r=1:task.conf.rpt, .combine=rbind, .export = ls(.GlobalEnv)) %dopar% {
        setwd(work.directory)
        slaver.function(r, task_config)
      }
      stopCluster(cluster.handler)
  }
  # 预处理多次模拟的结果
  if(!is.null(task_config$algorithm2.conf)) {
    # 如果有两个算法，则计算第一个算法的协方差值，第二个算法的协方差值。
    # 以及它们的差的协方差值。
    colcnt <- ncol(loss.estimator.matrix)
    if( colcnt %% 4 != 0) stop("invalid result")
    unit <- colcnt /4
    matrix.first <- loss.estimator.matrix[, 1:unit]
    matrix.second <- loss.estimator.matrix[, (unit+1):(2*unit)]
    matrix.third <- loss.estimator.matrix[, (2*unit+1):(3*unit)]
    matrix.fourth <- loss.estimator.matrix[, (3*unit+1):(4*unit)]
    param.first <- covariance_simulation.compute_covariance_parameters(matrix.first, matrix.second, task_config)
    param.second <-covariance_simulation.compute_covariance_parameters(matrix.third, matrix.fourth, task_config)
    param.diff <- covariance_simulation.compute_covariance_parameters(matrix.first-matrix.third, matrix.second-matrix.fourth, task_config)
    return(list("loss"=loss.estimator.matrix,  "param" = c(param.first, param.second, param.diff)))
  } else {
    colcnt <- ncol(loss.estimator.matrix  )
    if( colcnt %% 2 != 0) stop("invalid result")
    unit <- colcnt /2
    unit <- colcnt /2
    matrix.first <- loss.estimator.matrix[, 1:unit]
    matrix.second <- loss.estimator.matrix[, (unit+1):(2*unit)]
    param.first <- covariance_simulation.compute_covariance_parameters(matrix.first, matrix.second, task_config)
    return(list("loss"= loss.estimator.matrix, "param" = param.first))
  }
}

covariance_simulation.write_output <- function(result, task_config) {
  
}