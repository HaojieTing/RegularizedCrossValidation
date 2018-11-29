# 测试模拟在Letter数据上的协方差。
#
# Author: Wangruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2018/02/03


runitCovarianceSimulationOnLetter <- function(nsize, w, overlap.count, repi = 1000) {
  source("./tasks/covariance_simulation.R", encoding="UTF-8")
  task_config <- list(
    dataset.conf = list(
      name = "uci_letter",
      type = "classification",
      binarize = TRUE,
      samplingConf = list(
        n = nsize
      )
    ),
    algorithm1.conf = list(
      name = "classificationTree",
      type = "classification",
      method = "class"
    ),
    algorithm2.conf = list(
      name = "firstNearestNeighborhood",
      type = "classification",
      w = w
    ),
    crossvalidation.conf = list(
      name = "hold_out",
      n1 = nsize/2,
      n = nsize
    ),
    overlap_count = overlap.count,
    rpt = repi,
    seq = FALSE,
    cores = 20
  )
  task_config <- covariance_simulation.task_config_validation(task_config)
  result <- covariance_simulation.perform_task(task_config)
  return(result)
}

runitCovarianceSimulationOnLetterMultipleWOnLetter <- function(nsize, repi) {
  ws <- seq(1, 100, 1)
  results <- c()
  for (w in ws) {
    result <- runitCovarianceSimulationOnLetter(nsize, w, nsize/4, repi)
    results <- rbind(results, result)
  }
  row.names(results) <- ws
  write.csv(results, file = paste("test_result/Cov_sim_Letter_", "nsize", nsize, "rep", repi, "csv", sep = "."))
}


runitCovarianceSimulationOnLetterMultipleWOnLetter_zero <- function(nsize, repi) {
  ws <- seq(1, 100, 1)
  results <- c()
  for (w in ws) {
    result <- runitCovarianceSimulationOnLetter(nsize, w, 0, repi)
    results <- rbind(results, result)
  }
  row.names(results) <- ws
  write.csv(results, file = paste("test_result/Zero.Cov_sim_Letter_", "nsize", nsize, "rep", repi, "csv", sep = "."))
}