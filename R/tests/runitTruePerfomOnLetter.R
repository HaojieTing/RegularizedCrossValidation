# 在Letter数据上测试序贯检验。
#
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2018/2/3

runitTruePerformOnLetter <- function(w) {
  # 测试Letter数据上的真实性能。
  # w 从1取到100.
  source("./tasks/true_performance_simulation.R", encoding="UTF-8")
  letter_true_perf_conf <- list(
    alpha = 0.05,
    rep = 1000,
    close_form = TRUE,
    n1 = 150,
    seq = FALSE,
    cores = 20,
    dataset.conf = list(
      name = "uci_letter",
      type = "classification",
      binarize = TRUE,
      samplingConf = list(
        n = 300
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
    )
  )
  task_config <- true_performance_simulation.validate_config(letter_true_perf_conf)
  result <- true_performance_simulation.perform_task(task_config)
  result <- c(result[[1]], result[[2]], result[[3]])
  return(result)
}

runitTruePerformOnLetterOnAllW <- function() {
  ws <- seq(1, 100, 1)
  results <- c()
  for(w in ws) {
    result <- runitTruePerformOnLetter(w)
    results <- rbind(results, result)
  }
  row.names(results) <- ws
  write.csv(results, file = "./test_result/true_perform_on_letter.csv")
  return(results)
}

