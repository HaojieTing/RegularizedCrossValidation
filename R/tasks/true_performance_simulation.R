# 算法真实性能模拟任务。
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/11/17

estimateBeta_Delta_Pi <- function(mumat) { 
  # 估计\beta, \Delta和\pi的值。
  #
  # Args:
  #   mumat: 损失函数矩阵。
  # Returns: 
  #   三个参数的估计值.
  T_mean <- rowMeans(mumat)
  parameter.values <- rep(NA, 3)
  names(parameter.values) <- c("beta","delta","pi")  
  parameter.values['beta'] <- mean(T_mean)
  parameter.values['delta'] <- mean((mumat - parameter.values['beta'])**2)
  K <- ncol(mumat)
  parameter.values['pi'] <- ((mean((T_mean - parameter.values['beta'])**2) / parameter.values['delta']) - (1/K))/(1-(1/K))
  return(parameter.values)
}


g_theta <- function(muv, paras){
  # 计算g_theta的值。
  # 
  # Args: 
  #     muv: 泛化误差估计向量。
  #     paras: 参数值。
  # Returns:
  #    g_theta的值。
  gparas <- rep(NA, 3)
  mn <- mean(muv)
  gparas[1] <- mn - paras['beta']
  gparas[2] <- sum((muv-paras['beta'])**2 - paras['delta'])
  gparas[3] <- (mn - paras['beta']) ** 2 - paras['delta'] * (paras['pi'] + (1-paras['pi']/length(muv)))
  return(gparas)
}


V_theta <- function(mumat, paras){
  g_theta_all <- apply(mumat, 1, g_theta, paras)
  B_theta <- g_theta_all %*% t(g_theta_all)
  A_theta <- matrix(rep(0, 9),3)
  A_theta[1,1] <- 1
  K <- ncol(mumat)
  A_theta[2,2] <- K
  A_theta[3,2] <- paras['pi'] + (1-paras['pi'])/K
  A_theta[3,3] <- (K-1)/K * paras['delta']
  A_theta <- nrow(mumat) * A_theta
  #计算V_theta
  A_theta_inverse <- solve(A_theta)
  V_theta <- A_theta_inverse %*% B_theta %*% t(A_theta_inverse)
  return(diag(V_theta))
}

confInteval <- function(paras, v_thetas, alpha){
  quantile = qnorm(1-alpha/2)
  confInt <- matrix(rep(NA, 9),3)
  confInt[1,] <- paras - quantile * sqrt(v_thetas)
  confInt[2,] <- paras + quantile * sqrt(v_thetas)
  confInt[3,] <- (confInt[1,] + confInt[2, ]) /2 
  colnames(confInt) <- names(paras)
  row.names(confInt) <- c("Lower","Upper","Mean")
  return(t(confInt))
}

estimateBetaTrueConfIntByCloseForm <- function(muvalues, rpt, alpha) {
  if( nrow(muvalues) != rpt) {
    stop("")
  }
  T_i_bar <- rowMeans(muvalues)
  T_bar   <- mean(T_i_bar)
  sd 		<- sqrt(mean((T_i_bar - T_bar)^2)/rpt)
  quantile <- qnorm(1-alpha/2)
  confInt <- rep(NA, 4)
  names(confInt) <- c("Lower","Upper","Mean", "sd")
  confInt[1] <- T_bar - quantile *  sd
  confInt[2] <- T_bar + quantile * sd
  confInt[3] <- T_bar
  confInt[4] <- sd
  return(confInt)
}

estimateTrueConfInteval <- function(muvalues, rpt, alpha){
  if( nrow(muvalues) != rpt) {
    stop(paste("", nrow(muvalues), rpt))
  }
  mumat <- muvalues
  paras <- estimateBeta_Delta_Pi(mumat)
  v_thetas <- V_theta(mumat, paras)
  ci <- confInteval(paras, v_thetas, alpha)
  return(ci)
}

slaver.function <- function(rep_index, task_config, dataset.generator, 
                            algorithm1.generator, algorithm2.generator) {
  # 估计泛化误差的损失向量。
  #
  # Args:
  #   rep_index: 重复次数的索引值。
  # Returns:
  #   损失值向量。
  metric.conf <- task_config$metric.conf
  metric.entry <- LoadPerformanceMetricGenerator(metric.conf)[[1]]
  dataset.conf <- task_config$dataset.conf
  algorithm1.conf <- task_config$algorithm1.conf
  algorithm2.conf <- task_config$algorithm2.conf
  #set.seed(rep_index)
  dataset.object <- LoadDataSetGenerator(dataset.conf)
  dataset.generator <- dataset.object[[1]]
  dataset.packages <- dataset.object[[2]]
  WorkerInit(dataset.packages)
  dataset.observants <- dataset.generator(dataset.conf) 
  crossvalidation.conf <- NULL
  if(!is.null(task_config$prop)) {
    crossvalidation.conf <- list(
      name = "hold_out",
      prop = task_config$prop,
      data = dataset.observants
    )
  } else if(!is.null(task_config$n1)) {
    crossvalidation.conf <- list(
      name = "hold_out",
      n1 = task_config$n1,
      data = dataset.observants
    )
  } else {
    stop("invalid cross validation config")
  }
  crossvalidation.info <- LoadCrossValidationGenerator(crossvalidation.conf$name)
  crossvalidation.generator <- crossvalidation.info[[1]]
  crossvalidation.packages <- crossvalidation.info[[2]]
  WorkerInit(crossvalidation.packages)
  algorithm1.info <- LoadAlgorithmGenerator(algorithm1.conf)
  algorithm1.generator <- algorithm1.info[[1]]
  algorithm1.packages <- algorithm1.info[[2]]
  WorkerInit(algorithm1.packages)
  cv.object.first <- crossvalidation.generator(crossvalidation.conf)
  partition.first <- cv.object.first$partitions
  pe.first  <- TrainAndTestForHoldoutEstimator(dataset.observants, partition.first, algorithm1.generator, algorithm1.conf, metric.conf=metric.conf, metric.entry=metric.entry)
  loss.vector <- pe.first[[2]]
  if(exists("algorithm2.conf")) {
    algorithm2.info <- LoadAlgorithmGenerator(algorithm2.conf)
    algorithm2.generator <- algorithm2.info[[1]]
    algorithm2.packages <- algorithm2.info[[2]]
    WorkerInit(algorithm2.packages)
    pe.second  <- TrainAndTestForHoldoutEstimator(dataset.observants, partition.first, algorithm2.generator, algorithm2.conf, metric.conf=metric.conf, metric.entry=metric.entry)
    loss.vector <- c(loss.vector, pe.second[[2]])
  }
  loss.vector
}


true_performance_simulation.validate_config <- function(task_config) {
  source("./utils.R", encoding = "UTF-8")
  source("./datasets/data_loader.R", encoding = "UTF-8")
  source("./crossvalidations/cv_loader.R", encoding="UTF-8")
  source("./mlAlgorithms/algor_loader.R", encoding="UTF-8")
  source("./model_train_and_predict.R", encoding = "UTF-8")
  source("./metrics/metric_loader.R", encoding="UTF-8")
  if (is.null(task_config)) stop("The task_config load failed.")
  if (is.null(task_config$alpha) )     stop("Please specify confidence probability [default is 0.05].")
  if (is.null(task_config$rep))        stop("Please specify data repetition count [default is 1000].")
  if (is.null(task_config$close_form)) task_config[["close_form"]] <- TRUE
  if (is.null(task_config$n1))         stop("Please specify training set size.")
  if (is.null(task_config$prop))      task_config$prop <- NULL
  if (is.null(task_config$seq))        task_config[["seq"]] <- TRUE
  if (is.null(task_config$dataset.conf))    stop("Please specify data set configuration.")
  task_config$dataset.conf <- ValidateAndResolveDataSetConfiguration(task_config$dataset.conf)
  if (is.null(task_config$algorithm1.conf)) stop("Please specify the first algorithm configuration.")
  task_config$algorithm1.conf <- ValidateAndResolveAlgorithmConfiguration(task_config$algorithm1.conf)
  # 这里不对algorithm2.conf做存在性验证。
  if (!is.null(task_config$algorithm2.conf)) {
    task_config$algorithm2.conf <- ValidateAndResolveAlgorithmConfiguration(task_config$algorithm2.conf)
  }
  return(task_config)
}


cl <- NULL
cores <- NULL 

true_performance_simulation.perform_task <- function(task_config) {
  dataset.info <- LoadDataSetGenerator(task_config$dataset.conf) 
  dataset.generator <- dataset.info[[1]]
  dataset.packages <- dataset.info[[2]]
  algorithm1.info <- LoadAlgorithmGenerator(task_config$algorithm1.conf)
  algorithm1.generator <- algorithm1.info[[1]]
  algorithm1.packages <- algorithm1.info[[2]]
  algorithm2.generator <- NULL
  algorithm2.packages <- NULL
  if (!is.null(task_config$algorithm2.conf)) {
    algorithm2.info <- LoadAlgorithmGenerator(task_config$algorithm2.conf)
    algorithm2.generator <- algorithm2.info[[1]]
    algorithm2.packages <- algorithm2.info[[2]]
  } 
  loss.matrix <- c()
  if (task_config[["seq"]]) {
    WorkerInit(dataset.packages)
    WorkerInit(algorithm1.packages)
    if(exists("algorithm2.conf")) WorkerInit(algorithm2.packages)
    for(r in 1:task_config$rep) {
      result <- slaver.function(r, task_config, dataset.generator, 
                      algorithm1.generator, algorithm2.generator)
      loss.matrix <- rbind(loss.matrix, result) 
    }
  } else {
      # 多核处理该程序
      require(foreach)
      library(doParallel)
      cl <- makeCluster(type="MPI")
      registerDoParallel(cl)
      work.directory <- getwd()
      loss.matrix <- foreach(r=1:task_config$rep, .combine=rbind, .export = ls( .GlobalEnv)) %dopar% {
         setwd(work.directory)                       
         slaver.function(r, task_config, dataset.generator, algorithm1.generator, algorithm2.generator)
      }
      stopCluster(cl)
  }
  nalgors <- 1
  if(!is.null(task_config$algorithm2.conf)) nalgors <- 2
  n2.size <- ncol(loss.matrix) / nalgors
  if(task_config$close_form) {
    # 以解析解的形式计算beta的置信区间。
    result.algorithm1 <- estimateBetaTrueConfIntByCloseForm(loss.matrix[,1:n2.size], 
                                                            task_config$rep, task_config$alpha)
    if(!is.null(task_config$algorithm2.conf)){
      result.algorithm2 <- estimateBetaTrueConfIntByCloseForm(loss.matrix[,(1+n2.size):(2*n2.size)], 
                                                              task_config$rep, task_config$alpha)
      diff.matrix = loss.matrix[,(1+n2.size):(2*n2.size)] - loss.matrix[,1:n2.size]
      result.algorithm.diff <- estimateBetaTrueConfIntByCloseForm(diff.matrix, task_config$rep, 
                                                                  task_config$alpha)
    }
  } else {
    # 代入一般公式计算beta, delta, pi的置信区间。
    result.algorithm1 <- estimateTrueConfInteval(loss.matrix[,1:n2.size], 
                                                 task_config$rep, task_config$alpha)
    if(!is.null(task_config$algorithm2.conf)){
      result.algorithm2 <- estimateTrueConfInteval(loss.matrix[,(1+n2.size):(2*n2.size)], 
                                                   task_config$rep, task_config$alpha)
      diff.matrix = loss.matrix[,(1+n2.size):(2*n2.size)] - loss.matrix[,1:n2.size]
      result.algorithm.diff <- estimateTrueConfInteval(diff.matrix, task_config$rep, 
                                                       task_config$alpha)
    }
  }
    return(list(result.algorithm1, result.algorithm2, result.algorithm.diff))
}

true_performance_simulation.output_result <- function(result, task_config) {
  
}