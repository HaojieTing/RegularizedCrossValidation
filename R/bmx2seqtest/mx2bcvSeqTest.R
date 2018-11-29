# mx2bcv序贯t-检验的主程序模块。
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/11/1

# 生成要检验的数据集

mx2bcvseqtest <- function (dataConf, algorConf, algorConf2, cvConf, conf_TASK_BMX2SEQTEST ) {

  alpha <- conf_TASK_BMX2SEQTEST$alpha  # 第一类错误概率
  beta  <- conf_TASK_BMX2SEQTEST$beta   # 第二类错误概率
  veConf <- conf_TASK_BMX2SEQTEST$var_est_conf  # 方差估计配置
  delta_0 <- conf_TASK_BMX2SEQTEST$delta_0  # 原假设中的delta0
  delta_1 <- conf_TASK_BMX2SEQTEST$delta_1  # 备择假设中的delta1
  upper_m <- conf_TASK_BMX2SEQTEST$upper_m  # m参数停止的上界
  lower_m <- 3  # m参数的起始值
  if(!is.null(conf_TASK_BMX2SEQTEST$lower_m)) {
    lower_m <- conf_TASK_BMX2SEQTEST$lower_m 
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
  if(cvConf$name != 'increaseBalancedMx2cv') {
    stop("cross validation needed: increaseBalancedMx2cv");
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
  if(nrow(data)>1000)
    data <- data[sample(1:nrow(data),1000), ]
  n <- nrow(data)    
  cvConf$data <- data  
  cur_m <- lower_m  # 存储当前的m值  
  test_result <- 2
  muv1 <- c()  # 存储第一个机器学习算法的性能
  muv2 <- c()  # 存储第二个机器学习算法的性能
  name.vec <- c("m", "delta_0", "delta_1", "var_est", "I_l", "I_r", "mu_algor1","mu_algor2","mu_diff", "prob_rej_H0", "prob_rej_H1")
  verbose.table <- c()
  for(cur_m in lower_m:upper_m) {
      # 计算c_m和f_m的估计
      c_m_est = sqrt((2*cur_m+1)/(2*cur_m-1)) # 统计量所服从的t分布前的常数
      f_m_est = 2/3*(2*cur_m-1)  # 统计量所服从的t分布的自由度
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
      mu_diff <- mean(mu_diff_vec)
      # 计算机器学习算法性能估计的方差估计
      ve.estimator <- loadVarEstForOneExprInfo(veConf$name)
      var_est <- ve.estimator(c(mu_diff_vec, mu_diff), cvConf$m, veConf)
      # 计算左右边界
      I_l = delta_0 - c_m_est * var_est * qt(p = 1-alpha, f_m_est)
      I_r = delta_1 + c_m_est * var_est * qt(p = 1-beta, f_m_est)
      prob_rej_H0 <- pt((mu_diff - delta_0)/(sqrt(var_est)*c_m_est), df = f_m_est, lower.tail = F)
      prob_rej_H1 <- pt((mu_diff - delta_1)/(sqrt(var_est)*c_m_est), df = f_m_est, lower.tail = T)
      verbose.table <- rbind(verbose.table, c(cur_m, delta_0, delta_1, var_est, I_l, I_r, mean(muv1), mean(muv2), mu_diff, prob_rej_H0, prob_rej_H1))
      if(mu_diff > I_r) { # 接受H1
        test_result <- 1
        break()
      } else if(mu_diff < I_l) { # 接受H0
        test_result <- 0
       break()
      } else { # 无法判断，进入下一个循环
        test_result <- 2
      }
  }
  colnames(verbose.table) <- name.vec
  test_result <- t(c(cur_m, test_result, prob_rej_H0, prob_rej_H1))
  colnames(test_result) <- c("m.stop", "test.result","prob.rej.H0","prob.rej.H1")
  result <- list(
    "verbose" = verbose.table,
    "test.result" = test_result
  )
  return(result)
}