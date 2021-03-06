# 模拟BRHS中的rho值。


# sed -i s/150/20/g `grep 150 -rl --include="rhoWYneco2014Expr2_40_svm.R" ./`

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
  print(result$param)
}

testCovarianceSimulationBinaryLetter <- function(nsize) {
  source("tasks/covariance_simulation.R", encoding = "UTF-8")  
  config <- list(
    rpt = 100000,
    seq = F,
    overlap_count = round(nsize/6),
    dataset.conf = list(
      name = "uci_letter",
      type = "classification",
      binarize = T,
      samplingConf = list(
        n = nsize
      )
    ),
    algorithm1.conf = list(
      name = "tree",
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
  print(result$param)
}

testCovarianceSimulationOth <- function(nsize, algorname) {
  algorithm.confs <- list(
    svm = list(
      name = "svm",
      type = "classification"
    ),
    nn  = list(
      name = "nnet",
      type = "classification"
    ),
    nb  = list(
      name = "naiveBayes",
      type = "classification"
    ),
    cr  = list(
      name = "tree",
      type = "classification"
    )
  )
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
    algorithm1.conf = algorithm.confs[[algorname]],
    crossvalidation.conf = list(
      name = "hold_out",
      n1 = round(nsize/2),
      n = nsize
    )
  )
  covariance_simulation.task_config_validation(config)
  result <- covariance_simulation.perform_task(config)
  print(paste("data set size:", nsize))
  print(result$param)
}