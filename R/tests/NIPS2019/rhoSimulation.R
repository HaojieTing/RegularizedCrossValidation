# 模拟BRHS中的rho值。

testCovarianceSimulation <- function() {
  source("tasks/covariance_simulation.R", encoding = "UTF-8")  
  config <- list(
    rpt = 1000,
    seq = FALSE,
    overlap_count = round(150/6),
    dataset.conf = list(
      name = "simWYneco2014Expr2",
      type = "classification",
      n = 150
    ),
    algorithm1.conf = list(
      name = "logisticRegr",
      type = "classification"
    ),
    crossvalidation.conf = list(
      name = "hold_out",
      prop = 0.5
    )
  )
  covariance_simulation.task_config_validation(config)
  result <- covariance_simulation.perform_task(config)
  print(result)
}