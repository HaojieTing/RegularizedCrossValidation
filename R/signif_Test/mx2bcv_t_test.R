# 对比两个有监督算法性能的mx2 bcv t检验.
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2019/4/28

mx2bcv_t_test.task_config_validation <- function(task_config) {
  # 验证brhs检验任务配置的正确性。
  source("varianceEstimator/var_est_loader.R", encoding = "UTF-8")
  source("utils.R", encoding="UTF-8")
  # 重复次数m
  if(is.null(task_config$m)) stop("m is not provided.")
  # 置信概率
  if(is.null(task_config$alpha)) stop("The confidence probability is not provided")
  # 原假设中的delta
  if(is.null(task_config$delta)) stop("The delta in the hypotheses is not provided")
  # 方差估计
  if(is.null(task_config$var.est.conf)) stop("The variance estimation configuration is not provided")
  veConf <- task_config$var.est.conf
  if(veConf$name != "var_est3_mx2cv") stop("the type of var estimation is incorrect.")
  # mu的真实值
  #if(is.null(task_config$mu)) stop("The true value of mu is not provided.")
  # 提供的泛化误差估计文件
  if(is.null(task_config$mu.vec)) stop("Please specify the vector of BRHS estimator of the generalization error")
  if(length(task_config$mu.vec) != 18) stop("Length of mu vector is mis-matched.")
  return(task_config)
}

mx2bcv_t_test.perform_task <- function(task_config) {
  # 执行BRHS t检验。
  mu.vec <- task_config$mu.vec
  alpha  <- task_config$alpha
  veConf <- task_config$var.est.conf
  m <- task_config$var.est.conf$m
  mu.vec.algor1 <- mu.vec[1:(2*m)]
  mu.vec.algor2 <- mu.vec[(2*m+1):(4*m)]
  mu.vec.diff   <- mu.vec[(4*m+1):(6*m)]
  mu.diff.mean <- mean(mu.vec.diff)
  delta <- task_config$delta
  ve.estimator <- loadVarEstForOneExprInfo(veConf$name)
  var.est <- ve.estimator(c(mu.vec.diff, mu.diff.mean), veConf)
  # 计算检验统计量
  test.val <- abs(mu.diff.mean-delta)/sqrt(var.est)
  print(paste(mu.diff.mean, var.est, test.val))
  if(mu.diff.mean-delta == 0) return(1)
  test.quantile <- qt(1-alpha/2.0, 2*m-1)
  if(test.val > test.quantile) {
    return(1)
  }
  return(0)
}