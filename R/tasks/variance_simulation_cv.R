# 基于交叉验证的方差模拟任务模块的相关函数.
#
# 给定数据集、算法、交叉验证、评价指标，模拟
# 交叉验证估计的方差。在模拟时，支持如下几种情形：
# 1. 单个算法、单个数据集、单个交叉验证、多种或单个
#    评价指标的方差模拟。
# 2. 单个数据集，多个算法、单个交叉验证、多种或单个
#    评价指标的方差模拟。
# 3. 两个算法的差值的方差函数。
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/11/17

slaver.function <- function(rep_idx, task_config){
  # 在每个计算节点上产生单个数据集，
  # 并计算交叉验证切分集。进而计算
  # 数据集上的交叉验证估计。
  #
  # Args:
  #   repetition.serial.no: 重复次数的序号，即，对应第几次重复。
  # Returns:
  #   每一个算法拟合出来的估计值.
  # 加载数据集的生成函数和附属包
  rep.tot = task_config$data_rep_cnt * task_config$part_set_rep_cnt
  dataset.conf <- task_config$dataset.conf
  dataset.info <- LoadDataSetGenerator(dataset.conf) 
  dataset.generator <- dataset.info[[1]]
  dataset.packages <- dataset.info[[2]]
  WorkerInit(dataset.packages)
  # 加载交叉验证方法的生成函数和附属包
  crossvalidation.conf <- task_config$crossvalidation.conf
  crossvalidation.info <- LoadCrossValidationGenerator(crossvalidation.conf$name)
  crossvalidation.generator <- crossvalidation.info[[1]]
  crossvalidation.packages <- crossvalidation.info[[2]]
  WorkerInit(crossvalidation.packages)
  # 设置种子，并生成数据集
  dataset.index <- ((rep_idx - 1) %% rep.tot) + 1  # 对应的是第几组数据集
  partitionset.index <- (rep_idx - 1) %/% rep.tot   # 对应的是第几组切分集
  # 产生数据集
  dataset.seed <- 0
  partitionset.seed <- 1111111
  if(!is.null(task_config$dataset.seed))
    dataset.seed <- task_config$dataset.seed
  if(!is.null(task_config$partitionset.seed))
    partitionset.seed <- task_config$partitionset.seed
  set.seed(dataset.index + dataset.seed)  # 数据集产生的种子，通过设置为dataset.index, 可保证产生相同的数据集.
  dataset <- dataset.generator(dataset.conf)
  crossvalidation.conf$data <- dataset #付给交叉验证配置.
  dataset.size <- nrow(data)
  # 产生切分集
  if(task_config$rnd_seed){
    set.seed(partitionset.index + partitionset.seed)  # 其中， 1111111是固定加入的一个常数，无特定意义。
  }
  cv.object <- crossvalidation.generator(crossvalidation.conf)
  partitionset <- cv.object$partitions
  partitionset.size <- length(partitionset)
  # 训练并测试，得到交叉验证估计
  algorithm.names <- names(task_config$algorithm.confs)
  estimator.vector <- c()
  for(algor.name in algorithm.names) {
    algorithm.conf <- task_config$algorithm.confs[[algor.name]]
    # 加载机器学习算法的生成函数和附属包
    algorithm.info <- LoadAlgorithmGenerator(algorithm.conf)
    algorithm.generator <- algorithm.info[[1]]
    algorithm.packages <- algorithm.info[[2]]
    WorkerInit(algorithm.packages)
    crossvalidation.estimator.results <- TrainAndTestForCrossValidationEstimatorWithPartitionSet(dataset, partitionset, algorithm.generator, algorithm.conf)
    holdout.estimators <- crossvalidation.estimator.results[[2]]  # 取出所有holdout估计.
    crossvalidation.estimator <- crossvalidation.estimator.results[[1]]  # 取出交叉验证估计.
    # 定义估计向量，其中最后一列存交叉验证估计，余下存holdout估计
    estimator.vector <- c(estimator.vector, holdout.estimators)  # 存储holdout估计 
    estimator.vector <- c(estimator.vector, crossvalidation.estimator)  # 存储交叉验证估计.
  }                                                                                               
  estimator.vector
}

variance_simulation_cv.compute_variance <- function(estimator.matrix, task_config) {
  # 根据给出的估计，计算泛化误差估计的方差.
  # 
  # Args:
  #   estimator.matrix: 泛化误差估计的矩阵。
  #   task_config: 方差模拟任务的配置。
  # Returns:
  #   不同算法的方差结果，包括：
  #       - 总体方差
  #       - 给定切分后，数据变化引起的方差
  #           > mean.b.ps 给定切分集后，数据变化引起的泛化误差估计的均值
  #           > var.b.ps  给定切分集后，数据变化引起的泛化误差估计的方差
  #       - 给定数据后，切分变化引起的方差
  #           > mean.b.dt 给定数据集后，切分变化引起的泛化误差估计的均值
  #           > var.b.dt  给定数据集后，切分变化引起的泛化误差估计的方差
  #       - TS: Variance induced by training set.
  #       - PS: Variance induce by partition set.
  # 将矩阵中的所有NA缺失值，置为0.
  estimator.matrix[is.na(estimator.matrix) ] <- 0.0
  algorithm.names <- names(task_config$algorithm.confs)
  col_cnt <- ncol(estimator.matrix)
  if (col_cnt %% length(algorithm.names)) stop("the estimation matrix is broken")
  col_cnt.per.algor <- col_cnt / length(algorithm.names)
  # 取出实验配置中的数据重复次数和切分重复次数
  data.rep.cnt <- task_config$data_rep_cnt
  ps.rep.cnt   <- task_config$part_set_rep_cnt
  result <- c()
  for (i in 1:length(algorithm.names)) {
    cv.est.vector <- estimator.matrix[, i*col_cnt.per.algor]
    # 将算法的多次模拟的交叉验证估计转换成矩阵，矩阵的行
    # 对应一个数据集。矩阵的列对应于一个切分。
    cv.est.matrix <- matrix(cv.est.vector, data.rep.cnt)
    # 要计算的总方差
    var.total <- var(cv.est.vector)
    # 基于切分的条件期望和条件方差分解
    var.vec.b.ps  <-  apply(cv.est.matrix, 2, var)
    mean.vec.b.ps <-  colMeans(cv.est.matrix)
    mean.var.b.ps <-  mean(var.vec.b.ps)
    var.mean.b.ps <- var(mean.vec.b.ps)
    # 基于数据的条件期望和方差分解
    var.vec.b.data  <- apply(cv.est.matrix, 1, var)
    mean.vec.b.data <- rowMeans(cv.est.matrix)
    mean.var.b.data <- mean(var.vec.b.data)
    var.mean.b.data <- var(mean.vec.b.data)
    # 内部方差IS和外部方差
    ts <- 1/2*(var.mean.b.ps + mean.var.b.data)
    ps <- 1/2*(var.mean.b.data + mean.var.b.ps)
    one_result <- t(c(var.total, mean.var.b.ps, var.mean.b.ps, mean.var.b.data, var.mean.b.data, ts, ps))
    row.names(one_result) <- algorithm.names[i]
    result <- rbind(result, one_result)
  }
  colnames(result) <- c("total var", "E[Var[|PS]]", "Var[E[|PS]]","E[Var[|DT]]", "Var[E[|DT]]", "TS","PS")
  return(result)
}

variance_simulation_cv.compute_regularized_variance_estimator <- function(estimator.matrix, task_config, extra_config) {
  # 计算正则化方差的形式。
  #
  # 给定泛化误差估计，计算正则化方差形式。目前，支持三种形式：
  #    - 不做任何正则化
  #    - 直接加一个方差估计。
  #    - 直接加一个标准差估计。
  #    - 加一个置信区间右边界。
  #    - 自定义正则化项。
  # Args:
  #   estimator.matrix: 估计矩阵
  #   task_config: 任务配置
  #   extra_config: 额外的配置
  # Returns:
  #   泛化误差正则化估计的形式
  
  # 验证额外参数中的配置是否正确.
  reg.type <- extra_config$type
  if(!reg.type %in% names(variance_simulation_cv.reg_est)) stop("Please provide correct reg var type.")
  reg.func <-  variance_simulation_cv.reg_est[[reg.type]]
  result <- c()
  algorithm.names <- names(task_config$algorithm.confs)
  col_cnt.per.algor <- ncol(estimator.matrix)/length(algorithm.names)
  for (i in 1:length(algorithm.names)) {
    cv.est.matrix <- estimator.matrix[, ((i-1)*col_cnt.per.algor+1):(i*col_cnt.per.algor)]
    result <- rbind(result, t(apply(cv.est.matrix, 1, reg.func, extra_config)))
  }
  row.names(result) <- algorithm.names
  if(!is.null(extra_config$file.name)) {
    file.name <- extra_config$file.name
    save(result, task_config, file=paste(data.dir, file.name, sep=.Platform$file.sep))
  }
  return(t(result))
}

variance_simulation_cv.summary_reg_result <- function(reg.matrix, task_config, extra_config) {
  # 对于正则化矩阵进行算法选择，并统计被选择算法的次数。
  #
  # Args:
  #   reg.matrix: 正则化矩阵.
  #   task_config: 任务的配置.
  #   extra_config: 额外配置。
  # Returns:
  #   算法被选择次数的统计表。
  algorithm.names <- names(task_config$algorithm.confs)
  if(length(algorithm.names)<=1) stop("No necessity to process")
  filter.func <- which.min
  if(!is.null(extra_config$is_min) && !extra_config$is_min) filter.func <- which.max
  filter.result <- apply(reg.matrix, 1, filter.func)
  decisions <- algorithm.names[filter.result]
  result <- table(factor(decisions, levels=algorithm.names))
  if(!is.null(extra_config$file.name)) {
    file.name <- extra_config$file.name
    save(result, decisions, task_config, file=paste(data.dir, file.name, sep=.Platform$file.sep))
  }
  return(result)
}

variance_simulation_cv.reg_est_with_no_reg <- function(mu_vec, extra_config) {
  return(mu_vec[length(mu_vec)])
}

variance_simulation_cv.reg_est_with_var_est <- function(mu_vec, extra_config) {
  source("./varianceEstimator/var_est_loader.R", encoding="UTF-8")
  if(is.null(extra_config$var.est.conf)) stop("Please provide var est conf")
  veConf <- extra_config$var.est.conf
  ve.estimator <- loadVarEstForOneExprInfo(veConf$name)
  var_est <- ve.estimator(mu_vec, veConf)
  return(mu_vec[length(mu_vec)]+var_est)
}

variance_simulation_cv.reg_est_with_std_est <- function(mu_vec, extra_config) {
  source("./varianceEstimator/var_est_loader.R", encoding="UTF-8")
  if(is.null(extra_config$var.est.conf)) stop("Please provide var est conf")
  veConf <- extra_config$var.est.conf
  ve.estimator <- loadVarEstForOneExprInfo(veConf$name)
  var_est <- ve.estimator(mu_vec, veConf)
  return(mu_vec[length(mu_vec)]+sqrt(var_est))
}

variance_simulation_cv.reg_est_with_ci_r_bound <- function(mu_vec, extra_config) {
  source("./varianceEstimator/var_est_loader.R", encoding="UTF-8")
  if(is.null(extra_config$var.est.conf)) stop("Please provide var est conf")
  if(is.null(extra_config$constant)) stop("Please provided constant")
  veConf <- extra_config$var.est.conf
  ve.estimator <- loadVarEstForOneExprInfo(veConf$name)
  var_est <- ve.estimator(mu_vec, veConf)
  return(mu_vec[length(mu_vec)]+sqrt(var_est)*extra_config$constant)
}

variance_simulation_cv.reg_est_with_cust_fun <- function(mu_vec, extra_config) {
  if(is.null(extra_config$func)) stop("Please provided func")
  return(extra_config$func(mu_vec))
}


variance_simulation_cv.reg_est <- list(
  "no_reg"  = variance_simulation_cv.reg_est_with_no_reg,
  "var_est" = variance_simulation_cv.reg_est_with_var_est,
  "std_est" = variance_simulation_cv.reg_est_with_std_est,
  "ci_r_bound" = variance_simulation_cv.reg_est_with_ci_r_bound,
  "cust_fun" = variance_simulation_cv.reg_est_with_cust_fun
)



variance_simulation_cv.task_config_validation <- function(task_config) {
  # 验证方差模拟任务的配置是否正确.
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
  # 数据重复次数
  if (is.null(task_config$data_rep_cnt)) stop("Need to specify dataRepCount for variance simulation task.")
  # 切分重复次数
  if (is.null(task_config$part_set_rep_cnt)) stop("Need to specify partitionSetRepCount for variance simulation task.")
  # 模拟时随机种子的设置. TODO: 如何设置不同的种子？  
  if (is.null(task_config$rnd_seed)) { 
    task_config[["rnd_seed"]] <- TRUE
    warning("partitionSetRndSeed is not specified, default value is TRUE.")
  }
  # 数据集设置
  if (is.null(task_config$dataset.conf))    stop("Please specify data set configuration.")
  # 是否顺序执行
  if (is.null(task_config$seq))        task_config[["seq"]] <- FALSE
  task_config$dataset.conf <- ValidateAndResolveDataSetConfiguration(task_config$dataset.conf)
  # 算法设置，这里可支持多个算法，以列表的形式给出.
  if (is.null(task_config$algorithm.confs)) stop("Please specify configurations of needed algorithm.")
  task_config$algorithm.confs <- lapply(task_config$algorithm.confs, ValidateAndResolveAlgorithmConfiguration)  
  # 交叉验证配置
  if (is.null(task_config$crossvalidation.conf)) stop("Please specify the cross validation configuration.")
  return(task_config)
}

library(foreach)
variance_simulation_cv.perform_task <- function(task_config) {
  rep.tot = task_config$data_rep_cnt * task_config$part_set_rep_cnt
  if (task_config[["seq"]]) {
    ge.estimator.matrix <- foreach(r=1:rep.tot, .combine = rbind, 
                           .export = ls()) %do% 
      slaver.function(r, task_config)
  } else {  # 并行处理该任务。
    require(doSNOW) 
    all.funs.and.variables <- ls()
    cluster.handler <- makeCluster(type="MPI")
    registerDoSNOW(cluster.handler)
    export.ignore <- clusterExport(cluster.handler, ls(.GlobalEnv))
    start.value <- 0
    if(!is.null(task_config[["start_value"]]))
      start.value <- task_config[["start_value"]]
    work.directory <- getwd()
    ge.estimator.matrix <- foreach(r=(start.value+1):(start.value+rep.tot), .combine = rbind) %dopar% {
      setwd(work.directory)
      slaver.function(r, task_config)
    }
    stopCluster(cluster.handler)
  }
  var.res <- variance_simulation_cv.compute_variance(ge.estimator.matrix, task_config)
  return(list(ge.estimator.matrix, var.res))
}


variance_simulation_cv.write_output <- function(result, task_config) {
  # 存储实验结果
  # 将得到的实验结果，和任务配置存入result.rdata文件中。
  # Args:
  #   result: 获得的实验结果.
  #   task_config: 任务配置
  # Returns:
  #    None
  if(length(result) != 2) stop("The provided variance simulation result is broken.")
  save(result[[1]], task_config, file=paste(data.dir, "result.rdata", sep=.Platform$file.sep))
  print(result[[2]])
  write.csv2(x = result[[2]], file =paste(data.dir, "var.result.csv", sep=.Platform$file.sep) )
}

