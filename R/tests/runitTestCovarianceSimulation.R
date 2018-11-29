testCovarianceSimulationOnSREG <- function(mu_scale_y=0.1, n=2000, sim_count=1000, seq=T) {
  source("./tasks/covariance_simulation.R", encoding="UTF-8")
  task_config <- list(
    dataset.conf = list(
      name = "SimpleNormalDataSet",
      type = "regression",
      n = n,
      mu_x = 10, 
      mu_bias_y = 100,
      mu_scale_y = mu_scale_y,
      sigma_x = 1,
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
    ),
    crossvalidation.conf = list(
      name = "hold_out",
      n1 = n/2,
      n = n
    ),
    overlap_count = n/4,
    rpt = sim_count,
    seq = seq
  )
  task_config <- covariance_simulation.task_config_validation(task_config)
  result <- covariance_simulation.perform_task(task_config)
  write.table(result$loss, file = paste("cov.sim.sreg", mu_scale_y, n, sim_count,format(Sys.time(), "%Y%m%d%H%M%S"),sep="_"))
  write.csv(result$param, file=paste("test_result/cov.param.sreg", mu_scale_y, n, sim_count,format(Sys.time(), "%Y%m%d%H%M%S"),sep="_"))
  return(T)
}


testCovarianceSimulationOnSCLA3 <- function(mu=0.1, n=2000, sim_count=1000, seq=T) {
  source("./tasks/covariance_simulation.R", encoding="UTF-8")
  task_config <- list(
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim3",
      mu1 = rep(mu, 2),
      n0= n/2,
      n1 = n/2
    ),
    algorithm2.conf = list(
      name = "classificationTree",
      type = "classification",
      method = "class"
    ),
    algorithm1.conf = list (
      name = "linearRegrClassifier",
      type = "classification"
    ),
    crossvalidation.conf = list(
      name = "hold_out",
      n1 = n/2,
      n = n
    ),
    overlap_count = n/4,
    rpt = sim_count,
    seq = seq
  )
  task_config <- covariance_simulation.task_config_validation(task_config)
  result <- covariance_simulation.perform_task(task_config)
  write.table(result$loss, file = paste("cov.sim.scla", mu, n, sim_count,format(Sys.time(), "%Y%m%d%H%M%S"),sep="_"))
  write.csv(result$param, file=paste("test_result/cov.param.scla", mu, n, sim_count,format(Sys.time(), "%Y%m%d%H%M%S"),sep="_"))
  return(T)
}


testCovarianceSimulationOnSREG_allConfig <- function() {
  configs <- seq(0, 10, 0.1 )
  results <- c()
  data.size <- c(100, 200, 500, 800, 1000,1500, 2000, 3000, 5000)
  for(n in data.size) {
    for(conf in configs) {
      cat(conf, "...\n")
      result <- testCovarianceSimulationOnSREG(mu_scale_y = conf, n = n)
      result <- c(n, conf, result)
      results <- rbind(results, result)
    }
    write.csv(results, file = paste("SREG_results.", n ,".csv", sep = ""))
  }
  return(results)
} 


testCovarianceSimulationOnSREG_zero <- function(mu_scale_y=0.1, n=2000) {
  source("./tasks/covariance_simulation.R", encoding="UTF-8")
  task_config <- list(
    dataset.conf = list(
      name = "SimpleNormalDataSet",
      type = "regression",
      n = n,
      mu_x = 10, 
      mu_bias_y = 100,
      mu_scale_y = mu_scale_y,
      sigma_x = 1,
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
    ),
    crossvalidation.conf = list(
      name = "hold_out",
      n1 = n/2,
      n = n
    ),
    overlap_count = 0,
    rpt = 1000,
    seq = TRUE,
    multicore = TRUE,
    cores = 4
  )
  task_config <- covariance_simulation.task_config_validation(task_config)
  result <- covariance_simulation.perform_task(task_config)
  return(result)
}

testCovarianceSimulationOnSREG_allConfig_zero <- function() {
  configs <- seq(0, 10, 0.1 )
  results <- c()
  data.size <- c(100, 200, 500, 800, 1000,1500, 2000, 3000, 5000)
  for(n in data.size) {
    for(conf in configs) {
      cat(conf, "...\n")
      result <- testCovarianceSimulationOnSREG_zero(mu_scale_y = conf, n = n)
      result <- c(n, conf, result)
      results <- rbind(results, result)
    }
    write.csv(results, file = paste("SREG_results.", n ,"_zero.csv", sep = ""))
  }
  return(results)
} 


testCovarianceSimulationOnSCLA_zero <- function(multiplier=0.15, n=2000) {
  source("./tasks/covariance_simulation.R", encoding="UTF-8")
  task_config <- list(
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim3",
      mu1 = rep(multiplier,2),
      n0 = n/2,
      n1 = n-n/2
    ),
    algorithm2.conf = list(
      name = "classificationTree",
      type = "classification",
      method = "class"
    ),
    algorithm1.conf = list (
      name = "linearRegrClassifier",
      type = "classification"
    ),
    crossvalidation.conf = list(
      name = "hold_out",
      n1 = n/2,
      n = n
    ),
    overlap_count = 0,
    rpt = 10000,
    seq = FALSE,
    multicore = TRUE,
    cores = c(rep("node103", 20),rep("node104", 20),rep("node105", 20))
  )
  task_config <- covariance_simulation.task_config_validation(task_config)
  result <- covariance_simulation.perform_task(task_config)
  return(result)
}

testCovarianceSimulationOnSCLA_allConfig_zero <- function() {
  configs <- seq(0.2, 1, 0.01)
  configs <- c(0.15, configs)
  results <- c()
  data.size <- c(100, 200, 500, 800, 1000,1500, 2000, 3000, 5000)
  for(n in data.size) {
    for(conf in configs) {
      cat(conf, "...\n")
      result <- testCovarianceSimulationOnSCLA_zero(multiplier = conf, n = n)
      result <- c(n, conf, result)
      results <- rbind(results, result)
    }
    write.csv(results, file = paste("SCLA_results.", n ,"_zero.csv", sep = ""))
  }
  return(results)
} 


testCovarianceSimulationOnSCLA <- function(multiplier=0.15, n=2000) {
  source("./tasks/covariance_simulation.R", encoding="UTF-8")
  task_config <- list(
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim3",
      mu1 = rep(multiplier,2),
      n0 = n/2,
      n1 = n-n/2
    ),
    algorithm2.conf = list(
      name = "classificationTree",
      type = "classification",
      method = "class"
    ),
    algorithm1.conf = list (
      name = "linearRegrClassifier",
      type = "classification"
    ),
    crossvalidation.conf = list(
      name = "hold_out",
      n1 = n/2,
      n = n
    ),
    overlap_count = n/4,
    rpt = 1000,
    seq = TRUE
  )
  task_config <- covariance_simulation.task_config_validation(task_config)
  result <- covariance_simulation.perform_task(task_config)
  return(result)
}

testCovarianceSimulationOnSCLA_allConfig <- function() {
  configs <- seq(0.2, 1, 0.01)
  configs <- c(0.15, configs)
  results <- c()
  data.size <- c(100, 200, 500, 800, 1000,1500, 2000, 3000, 5000)
  for(n in data.size) {
    for(conf in configs) {
      cat(conf, "...\n")
      result <- testCovarianceSimulationOnSCLA(multiplier = conf, n = n)
      result <- c(n, conf, result)
      results <- rbind(results, result)
    }
    write.csv(results, file = paste("SCLA_results.", n ,".csv", sep = ""))
  }
  return(results)
} 


testCovarianceSimulationOnGaussian <- function() {
  source("./tasks/covariance_simulation.R", encoding="UTF-8")
  n <- 2000
  task_config <- list(
    dataset.conf = list(
      name = "simpleGaussianYDataSet",
      type = "regression",
      n = n,
      mu = 1,
      psi = 1
    ),
    algorithm1.conf = list(
      name = "simpleMeanRegression",
      type = "regression"
    ),
    #algorithm2.conf = list(
    #  name = "linearModel",
    #  type = "regression",
    #  no_intercept = FALSE
    #),
    crossvalidation.conf = list(
      name = "hold_out",
      n1 = n/2,
      n = n
    ),
    overlap_count = n/4,
    rpt = 10000,
    seq = FALSE,
    multicore = TRUE,
    cores = 4
  )
  denterm <- 8.0*n/(n^2+8.0*n-12)
  print(paste(n, denterm, (1.0/(2.0+denterm))))
  task_config <- covariance_simulation.task_config_validation(task_config)
  result <- covariance_simulation.perform_task(task_config)
  return(result)
}
