# 对比两个机器学习算法的 mxK cv sequential t-test
#
# 检验问题为：
#     H0: \mu < \Delta_0  vs   H1: \mu > \Delta_1
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/12/30

mxkcv_seq_correct_t_test.task_config_validation <- function(task_config) {
  # 验证序贯检验任务的配置是否正确.
  #
  # Args:
  #   task_config: 序贯检验的任务配置
  # Return:
  #   更新后的任务配置
  source("datasets/data_loader.R", encoding="UTF-8")
  source("mlAlgorithms/algor_loader.R", encoding="UTF-8")
  source("crossvalidations/cv_loader.R", encoding="UTF-8")
  source("varianceEstimator/var_est_loader.R", encoding = "UTF-8")
  source("model_train_and_predict.R", encoding="UTF-8")
  source("utils.R", encoding="UTF-8")
  # 第一类错误概率
  if(is.null(task_config$alpha)) stop("The type I prob is not provided")  
  # 第二类错误概率
  if(is.null(task_config$beta)) stop("The type II prob is not provided")
  # 原假设中的delta0
  if(is.null(task_config$delta_0)) stop("The delta_0 in H0 is not provided")
  # 备择假设中的delta1
  if(is.null(task_config$delta_1)) stop("The delta_1 in H1 is not provided")
  # m参数停止的上界
  if(is.null(task_config$upper_m))  stop("The upper_m in H1 is not provided")
  # K-折交叉验证的参数K
  if(is.null(task_config$K)) stop("The fold parameter K is not provided")
  # m参数停止的上界
  if(is.null(task_config$lower_m))  task_config$lower_m <- 3  # m参数的起始值
  # 开启模拟置信区间的真实值任务，而不是判决.
  if (is.null(task_config$sim_int)) task_config$sim_int <- FALSE 
  # 连续多少次判决一次，才认为停止。
  if(is.null(task_config$agree_cnt)) task_config$agree_cnt <- 3
  # 方差估计
  if(is.null(task_config$var.est.conf)) stop("The var est conf is not provided")
  # 数据集配置
  if (is.null(task_config$dataset.conf))    stop("Please specify data set configuration.")
  task_config$dataset.conf <- ValidateAndResolveDataSetConfiguration(task_config$dataset.conf)
  # 算法1配置
  if (is.null(task_config$algorithm1.conf)) stop("Please specify the first algorithm configuration.")
  task_config$algorithm1.conf <- ValidateAndResolveAlgorithmConfiguration(task_config$algorithm1.conf)
  # 算法2配置
  if (is.null(task_config$algorithm2.conf)) stop("Please specify the first algorithm configuration.")
  task_config$algorithm2.conf <- ValidateAndResolveAlgorithmConfiguration(task_config$algorithm2.conf)
  # 交叉验证配置
  if (is.null(task_config$crossvalidation.conf)) stop("Please specify the cross validation configuration.")
  return(task_config)
}

mxkcv_seq_correct_t_test.perform_task <- function(task_config) {
  dataConf  <- task_config$dataset.conf
  algorConf <- task_config$algorithm1.conf
  algorConf2<- task_config$algorithm2.conf
  cvConf    <- task_config$crossvalidation.conf
  agree_cnt <- task_config$agree_cnt
  K         <- task_config$K
  alpha <- task_config$alpha  # 第一类错误概率
  beta  <- task_config$beta   # 第二类错误概率
  veConf <- task_config$var.est.conf  # 方差估计配置
  delta_0 <- task_config$delta_0  # 原假设中的delta0
  delta_1 <- task_config$delta_1  # 备择假设中的delta1
  upper_m <- task_config$upper_m  # m参数停止的上界
  lower_m <- 3  # m参数的起始值
  if(!is.null(task_config$lower_m)) {
    lower_m <- task_config$lower_m 
  }
  DataInfo <- LoadDataSetGenerator(dataConf)
  DataGenerator <- DataInfo[[1]]
  DataPackages <- DataInfo[[2]]
  # 生成要检验的第一个算法
  AlgorInfo <- LoadAlgorithmGenerator(algorConf)
  AlgorGenerator <- AlgorInfo[[1]]
  AlgorPackages <- AlgorInfo[[2]]
  # 生成要检验的第二个算法
  AlgorInfo2 <- LoadAlgorithmGenerator(algorConf2)
  AlgorGenerator2 <- AlgorInfo2[[1]]
  AlgorPackages2 <- AlgorInfo2[[2]]
  # 交叉验证类型必须为增量式mx2bcv.
  if(cvConf$name != 'mx2bcv_inc' && cvConf$name != 'mxkrcv_inc') {
    stop("cross validation needed: increaseBalancedMx2cv || mxkrcv_inc");
  }
  # 生成要使用的交叉验证
  CrossValidationInfo <- LoadCrossValidationGenerator(crossvalidation.name = cvConf$name)
  CrossValidationGenerator <- CrossValidationInfo[[1]]
  CrossValidationPackages <- CrossValidationInfo[[2]]
  
  WorkerInit(DataPackages)
  WorkerInit(AlgorPackages)
  WorkerInit(AlgorPackages2)
  WorkerInit(CrossValidationPackages)
  
  cvConf_org <- cvConf
  data <- DataGenerator(dataConf)
  n <- nrow(data)    
  cvConf$data <- data  
  cur_m <- lower_m  # 存储当前的m值  
  test_result <- 2
  muv1 <- c()  # 存储第一个机器学习算法的性能
  muv2 <- c()  # 存储第二个机器学习算法的性能
  name.vec <- c("m", "delta_0", "delta_1", "var_est", "I_l", "I_r", "mu_algor1","mu_algor2","mu_diff", "prob_rej_H0", "prob_rej_H1")
  verbose.table <- c()
  pre_decision <- 2
  tmp_agree_idx = 0
  sim_int_result <- c()
  for(cur_m in lower_m:(upper_m+agree_cnt)) {
    # 计算机器学习算法的性能估计
    cvConf$m <- cur_m    
    cvConf <- CrossValidationGenerator(cvConf)
    partition.set <- cvConf$splits_new
    cvres1 <- TrainAndTestForCrossValidationEstimatorWithPartitionSet(data, partition.set, AlgorGenerator, algorConf)
    cvres2 <- TrainAndTestForCrossValidationEstimatorWithPartitionSet(data, partition.set, AlgorGenerator2, algorConf2)
    # 存储两个算法的性能估计值
    muv1 <- c(muv1, cvres1[[2]])
    muv2 <- c(muv2, cvres2[[2]])
    mu_diff_vec <- muv1 - muv2
    mu_diff <- mean(mu_diff_vec[1:K])
    # 计算机器学习算法性能估计的方差估计
    ve.estimator <- loadVarEstForOneExprInfo(veConf$name)
    var_est <- ve.estimator(c(mu_diff_vec, mu_diff), cvConf$m, veConf)
    # 计算左右边界
    I_l = delta_0 - sqrt(var_est) * qt(p = 1-alpha, cur_m * (K-1))
    I_r = delta_1 + sqrt(var_est) * qt(p = 1-beta, cur_m * (K-1))
    if(I_r < I_l) {
      warning("The estimation of confidence interval is invalid")
    }
    if(task_config$sim_int) {
      sim_int_result <- rbind(sim_int_result, t(c(I_l, I_r)))
      next()
    }
    prob_rej_H0 <- pt((mu_diff - delta_0)/(sqrt(var_est)), df = cur_m * (K-1), lower.tail = F)
    prob_rej_H1 <- pt((mu_diff - delta_1)/(sqrt(var_est)), df = cur_m * (K-1), lower.tail = T)
    verbose.table <- rbind(verbose.table, c(cur_m, delta_0, delta_1, var_est, I_l, I_r, mean(muv1), mean(muv2), mu_diff, prob_rej_H0, prob_rej_H1))
    if(mu_diff > I_r) { # 接受H1
      if(pre_decision == 1) {
        tmp_agree_idx = tmp_agree_idx + 1
      } else {
        tmp_agree_idx <- 1
      }
      pre_decision <- 1
    } else if(mu_diff < I_l) { # 接受H0
      if(pre_decision == 0) {
        tmp_agree_idx = tmp_agree_idx + 1
      } else {
        tmp_agree_idx <- 1
      }
      pre_decision <- 0
    } else { # 无法判断，进入下一个循环
      test_result <- 2
      tmp_agree_idx <- 0
    }
    if(tmp_agree_idx >= agree_cnt) {
      test_result <- pre_decision
      break()
    }
  }
  if(task_config$sim_int) {
    row.names(sim_int_result) <- lower_m:upper_m
    return(sim_int_result)
  }
  colnames(verbose.table) <- name.vec
  test_result <- t(c(cur_m, test_result, prob_rej_H0, prob_rej_H1))
  colnames(test_result) <- c("m.stop", "test.result","prob.rej.H0","prob.rej.H1")
  result <- list( "verbose" = verbose.table, "test.result" = test_result)
  return(result)
}

mxkcv_seq_correct_t_test.write_output <- function(result) {
  
}
