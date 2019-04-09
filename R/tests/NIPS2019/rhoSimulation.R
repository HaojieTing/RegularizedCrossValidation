# 模拟BRHS中的rho值。

testCovarianceSimulation <- function(nsize) {
  source("tasks/covariance_simulation.R", encoding = "UTF-8")  
  config <- list(
    rpt = 100000,
    seq = F,
    overlap_count = round(nsize/6),
    dataset.conf = list(
      name = "simWYneco2014Expr2",
      type = "classification",
      n = nsize
    ),
    algorithm1.conf = list(
      name = "logisticRegr",
      type = "classification"
    ),
    crossvalidation.conf = list(
      name = "hold_out",
      n1 = round(nsize/2),
      n = nsize
    )
  )
  covariance_simulation.task_config_validation(config)
  result <- covariance_simulation.perform_task(config)
  print(paste("data set size:", nsize))
  print(result)
}