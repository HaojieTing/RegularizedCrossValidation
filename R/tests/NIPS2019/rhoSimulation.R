# 模拟BRHS中的rho值。

testCovarianceSimulation <- function() {
  source("tasks/covariance_simulation.R", encoding = "UTF-8")  
  config <- list(
    rpt = 1000,
    seq = FALSE,
    dataset.conf = list(
      
    ),
    algorithm1.conf = list(
      
    ),
    algorithm2.conf = list(
      
    ),
    crossvalidation.conf = list(
      name = "hold_out"
    )
  )
  covariance_simulation.task_config_validation(config)
  covariance_simulation.perform_task(config)
}