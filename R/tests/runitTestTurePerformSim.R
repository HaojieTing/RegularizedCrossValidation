# 
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/11/13

testTruePerformanceSimulationSREG1 <- function () {
  source("./tasks/true_performance_simulation.R", encoding="UTF-8")
  conf_TASK_TRUECI <- list (
    alpha = 0.05,
    rep = 1000,
    close_form = TRUE,
    n1 = 100,
    seq = TRUE,
    dataset.conf = list(
      name = "SimpleNormalDataSet",
      type = "regression",
      n = 200,
      mu_x = 10, 
      mu_bias_y = 100,
      mu_scale_y = 1,
      sigma_x = 1,
      sigma_y = 97
    ),
    algorithm1.conf = list(
      name = "simpleMeanRegression",
      type = "regression"
    ),
    algorithm2.conf = list(
      name = "linearModel",
      type = "regression",
      no_intercept = FALSE
    )
  )
  task_config <- true_performance_simulation.validate_config(conf_TASK_TRUECI)
  result <- true_performance_simulation.perform_task(task_config)
  print(result)
  if(is.null(result)) return(FALSE)
  return(T)
}

testTruePerformanceSimulationSREG2 <- function () {
  source("./tasks/true_performance_simulation.R", encoding="UTF-8")
  conf_TASK_TRUECI <- list (
    alpha = 0.05,
    rep = 1000,
    close_form = TRUE,
    n1 = 100,
    seq = TRUE,
    dataset.conf = list(
      name = "SimpleNormalDataSet",
      type = "regression",
      n = 200,
      mu_x = 10, 
      mu_bias_y = 100,
      mu_scale_y = 2,
      sigma_x = 2,
      sigma_y = 64
    ),
    algorithm1.conf = list(
      name = "simpleMeanRegression",
      type = "regression"
    ),
    algorithm2.conf = list(
      name = "linearModel",
      type = "regression",
      no_intercept = FALSE
    )
  )
  task_config <- true_performance_simulation.validate_config(conf_TASK_TRUECI)
  result <- true_performance_simulation.perform_task(task_config)
  print(result)
  if(is.null(result)) return(FALSE)
  return(T)
}

testTruePerformanceSimulationSREG4 <- function () {
  source("./tasks/true_performance_simulation.R", encoding="UTF-8")
  conf_TASK_TRUECI <- list (
    alpha = 0.05,
    rep = 1000,
    close_form = TRUE,
    n1 = 1000,
    seq = TRUE,
    dataset.conf = list(
      name = "SimpleNormalDataSet",
      type = "regression",
      n = 2000,
      mu_x = 10, 
      mu_bias_y = 100,
      mu_scale_y = 0.1,
      sigma_x = 5,
      sigma_y = 9
    ),
    algorithm1.conf = list(
      name = "simpleMeanRegression",
      type = "regression"
    ),
    algorithm2.conf = list(
      name = "linearModel",
      type = "regression",
      no_intercept = FALSE
    )
  )
  task_config <- true_performance_simulation.validate_config(conf_TASK_TRUECI)
  result <- true_performance_simulation.perform_task(task_config)
  print(result)
  if(is.null(result)) return(FALSE)
  return(T)
}

testTruePerformanceSimulationSCLA1 <- function () {
  source("./tasks/true_performance_simulation.R", encoding="UTF-8")
  conf_TASK_TRUECI <- list (
    alpha = 0.05,
    rep = 1000,
    close_form = TRUE,
    n1 = 100,
    seq = TRUE,
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim1"
    ),
    algorithm1.conf = list(
      name = "classificationTree",
      type = "classification",
      method = "class"
    ),
    algorithm2.conf = list (
      name = "linearRegrClassifier",
      type = "classification"
    )
  )
  task_config <- true_performance_simulation.validate_config(conf_TASK_TRUECI)
  result <- true_performance_simulation.perform_task(task_config)
  print(result)
  if(is.null(result)) return(FALSE)
  return(T)
}

testTruePerformanceSimulationSCLA2<- function () {
  source("./tasks/true_performance_simulation.R", encoding="UTF-8")
  conf_TASK_TRUECI <- list (
    alpha = 0.05,
    rep = 1000,
    close_form = TRUE,
    n1 = 100,
    seq = TRUE,
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim2"
    ),
    algorithm1.conf = list(
      name = "classificationTree",
      type = "classification",
      method = "class"
    ),
    algorithm2.conf = list (
      name = "linearRegrClassifier",
      type = "classification"
    )
  )
  task_config <- true_performance_simulation.validate_config(conf_TASK_TRUECI)
  result <- true_performance_simulation.perform_task(task_config)
  print(result)
  if(is.null(result)) return(FALSE)
  return(T)
}

testTruePerformanceSimulationSCLA4<- function () {
  source("./tasks/true_performance_simulation.R", encoding="UTF-8")
  conf_TASK_TRUECI <- list (
    alpha = 0.05,
    rep = 1000,
    close_form = TRUE,
    n1 = 1000,
    seq = TRUE,
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim4"
    ),
    algorithm1.conf = list(
      name = "classificationTree",
      type = "classification",
      method = "class"
    ),
    algorithm2.conf = list (
      name = "linearRegrClassifier",
      type = "classification"
    )
  )
  task_config <- true_performance_simulation.validate_config(conf_TASK_TRUECI)
  result <- true_performance_simulation.perform_task(task_config)
  print(result)
  if(is.null(result)) return(FALSE)
  return(T)
}

testTruePerformanceSimulationSREG3 <- function (y_scale=0.1, n.size = 2000, repi=1000, metric.conf=NULL, seq = F) {
  source("./tasks/true_performance_simulation.R", encoding="UTF-8")
  conf_TASK_TRUECI <- list (
    alpha = 0.05,
    rep = repi,
    close_form = TRUE,
    n1 = n.size /2,
    seq = seq,
    metric.conf=metric.conf,
    dataset.conf = list(
      name = "SimpleNormalDataSet",
      type = "regression",
      n = n.size,
      mu_x = 10, 
      mu_bias_y = 100,
      mu_scale_y = y_scale,
      sigma_x = 1.0,
      sigma_y = 9.97
    ),
    algorithm1.conf = list(
      name = "simpleMeanRegression",
      type = "regression"
    ),
    algorithm2.conf = list(
      name = "linearModel",
      type = "regression",
      no_intercept = FALSE
    )
  )
  task_config <- true_performance_simulation.validate_config(conf_TASK_TRUECI)
  result <- true_performance_simulation.perform_task(task_config)
  rtn <- c(result[[1]],result[[2]],result[[3]])
  return(rtn)
}


testTruePerformanceSimulation_bolasso <- function (lambda=0.1, n.size = 2000, repi=1000, metric.conf=NULL) {
  source("./tasks/true_performance_simulation.R", encoding="UTF-8")
  conf_TASK_TRUECI <- list (
    alpha = 0.05,
    rep = repi,
    close_form = TRUE,
    n1 = n.size /2,
    seq = T,
    metric.conf=metric.conf,
    dataset.conf = list(
      name = "bolassoreg",
      type = "regression",
      n = n.size,
      p = 16, 
      r = 8,
      mu = 0.1
    ),
    algorithm1.conf = list(
      name = "linearModel",
      type = "regression",
      no_intercept = FALSE
    ),
    algorithm2.conf = list(
      name = "lmRidgeModel",
      type = "regression",
      lambda = lambda
    )
  )
  task_config <- true_performance_simulation.validate_config(conf_TASK_TRUECI)
  result <- true_performance_simulation.perform_task(task_config)
  rtn <- c(result[[1]],result[[2]],result[[3]])
  return(rtn)
}


testTruePerformanceSimulationSCLA3 <- function (multiplier=0.15) {
  source("./tasks/true_performance_simulation.R", encoding="UTF-8")
  conf_TASK_TRUECI <- list (
    alpha = 0.05,
    rep = 1000,
    close_form = TRUE,
    n1 = 1000,
    seq = F,
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim3",
      mu1=rep(multiplier,2)
    ),
    algorithm1.conf = list(
      name = "classificationTree",
      type = "classification",
      method = "class"
    ),
    algorithm2.conf = list (
      name = "linearRegrClassifier",
      type = "classification"
    )
  )
  task_config <- true_performance_simulation.validate_config(conf_TASK_TRUECI)
  result <- true_performance_simulation.perform_task(task_config)
  rtn <- c(result[[1]],result[[2]],result[[3]])
  return(rtn)
}

testTruePerformanceSimulationHdRegr <- function(beta) {
  source("./tasks/true_performance_simulation.R", encoding="UTF-8")
  conf_TASK_TRUECI <- list (
    alpha = 0.05,
    rep = 10,
    close_form = TRUE,
    n1 = 500,
    seq = TRUE,
    dataset.conf = list(
      name = "hdRegrDataFL",
      type = "regression",
      n = 1000,
      d = 300,
      beta = beta
    ),
    algorithm2.conf = list(
      name = "LassoModel",
      type = "regression",
      prop = 0.2
    ),
    algorithm1.conf = list (
      name = "lmRidgeModel",
      type = "regression"
    )
  )
  task_config <- true_performance_simulation.validate_config(conf_TASK_TRUECI)
  result <- true_performance_simulation.perform_task(task_config)
  rtn <- c(result[[1]],result[[2]],result[[3]])
  return(rtn)
}

testTruePerformanceSimulationSigmoidData <- function(coef.value) {
  source("./tasks/true_performance_simulation.R", encoding="UTF-8")
  conf_TASK_TRUECI <- list (
    alpha = 0.05,
    rep = 10,
    close_form = TRUE,
    n1 = 500,
    seq = TRUE,
    dataset.conf = list(
      name = "two_class_with_sigmoid_prob_YU_exampleII",
      type = "classification",
      n = 1000,
      coef = coef.value
    ),
    algorithm2.conf = list(
      name = "classificationTree",
      type = "classification",
      method = "class"
    ),
    algorithm1.conf = list (
      name = "logisticGLM",
      type = "classification"
    )
  )
  task_config <- true_performance_simulation.validate_config(conf_TASK_TRUECI)
  result <- true_performance_simulation.perform_task(task_config)
  rtn <- c(result[[1]],result[[2]],result[[3]])
  return(rtn)
}

testTruePerformanceSimulationregrOutlier <- function(var.const) {
  source("./tasks/true_performance_simulation.R", encoding="UTF-8")
  conf_TASK_TRUECI <- list (
    alpha = 0.05,
    rep = 1000,
    close_form = TRUE,
    n1 = 500,
    seq = F,
    dataset.conf = list(
      name = "regrDataBG_outliers",
      type = "regression",
      n = 1000,
      d = 30,
      p = 0.95,
      var2 = var.const
    ),
    algorithm1.conf = list(
      name = "simpleMeanRegression",
      type = "regression"
    ),
    algorithm2.conf = list (
      name = "linearModel",
      type = "regression",
      no_intercept=F
    )
  )
  task_config <- true_performance_simulation.validate_config(conf_TASK_TRUECI)
  result <- true_performance_simulation.perform_task(task_config)
  rtn <- c(result[[1]],result[[2]],result[[3]])
  return(rtn)
}


testTruePerformanceSimulationregrnoOutlier <- function(d=30) {
  source("./tasks/true_performance_simulation.R", encoding="UTF-8")
  conf_TASK_TRUECI <- list (
    alpha = 0.05,
    rep = 10,
    close_form = TRUE,
    n1 = 500,
    seq = F,
    dataset.conf = list(
      name = "regrDataBG_nooutliers",
      type = "regression",
      n = 1000,
      d = d
    ),
    algorithm1.conf = list(
      name = "simpleMeanRegression",
      type = "regression"
    ),
    algorithm2.conf = list (
      name = "linearModel",
      type = "regression",
      no_intercept=F
    )
  )
  task_config <- true_performance_simulation.validate_config(conf_TASK_TRUECI)
  result <- true_performance_simulation.perform_task(task_config)
  rtn <- c(result[[1]],result[[2]],result[[3]])
  return(rtn)
}

testAllPerformOnSREG <- function(start.value = 0, end.value=10, step.value=0.1, n.size = 2000, repi=1000) {
  configs <- seq(start.value, end.value, step.value)
  results <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  work.directory <- getwd()
  config.length <- length(configs)
  results <- foreach(i = 1: config.length, .combine = rbind, .export = ls(.GlobalEnv)) %dopar%  {
    setwd(work.directory)
    conf <- configs[i]
    sequ <- T
    testTruePerformanceSimulationSREG3(y_scale = conf, n.size = n.size, repi = repi, seq = sequ)
  }
  stopCluster(cl)
  write.csv(results, file = paste(paste("test_result/SREG_trueperf",start.value, end.value, step.value,n.size, sep="_"), "csv", sep = "."))
  return(results)
}

testAllPerformOnBolasso <- function(start.value=0, end.value = 100, step.value = 1, n.size=500, repi=1000) {
  configs <- seq(start.value, end.value, step.value)
  results <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  work.directory <- getwd()
  config.length <- length(configs)
  results <- foreach(i = 1: config.length, .combine = rbind, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    conf <- configs[i]
    result <- testTruePerformanceSimulation_bolasso(lambda = conf, n.size = n.size, repi = repi)
    result
  }
  stopCluster(cl)
  write.csv(results, file = paste(paste("test_result/bolasso_trueperf",start.value, end.value, step.value,n.size, sep="_"), "csv", sep = "."))
  return(results)
}

testAllPerformOnCLA <- function() {
  configs <- seq(0.2, 1, 0.01)
  configs <- c(0.15, configs)
  results <- c()
  for(conf in configs) {
    cat(conf, "...\n")
    result <- testTruePerformanceSimulationSCLA3(conf)
    results <- rbind(results, result)
  }
  write.csv(results, file = "SCLA_results.csv")
  return(results)
}