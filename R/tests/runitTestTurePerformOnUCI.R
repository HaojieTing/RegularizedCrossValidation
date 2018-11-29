#
#
#
#

testTruePerformSimulationOnUCIDataSets <- function(fold) {
  assign("last.warning", NULL, envir = baseenv())
  uci.datanames <- c("artificial","hayesroth","pima","australian",
                     "heart","balance","hepatitis","promoters","bupa","horse",
                     "satellite47","car","iris","spambase","cmc","ironosphere","spect","credit",
                     "krvskp","tae","cylinder","letter","tic_tac_toe","dermatology","magic","titanic",
                     "donors","mammographic","transfusion","ecoli","monk","vehicle","flags","nursery",
                     "vote","flare","optdigits","wave","german","page_block","wine","glass","parkinsons",
                     "yeast","haberman","pendigits","zoo")
  data.names <- paste("uci", uci.datanames, sep="_")
  algorithms.configs <- list(
    "max" = list(
      name = "maxClassifier",
      type = "classification"
    ),
    "knn" = list(
      name = "knn",
      type = "classification",
      k = 5,
      kernal = "triangular"
    ),
    "lda" = list(
      name = "LinearDiscriminantAnalysis",
      type = "classification"
    ),
    "svm" = list(
      name = "svm",
      type = "classification"
    ),
    "cart" = list(
      name = "classificationTree",
      type = "classification",
      method = "class"
    ),
    "nn" = list(
      name = "nnet",
      type = "classification",
      h_size = 10,
      range = 1,
      MaxNwts = 200000
    ),
    "maxent" = list(
      type = "classification",
      name = "maxent"
    ),
    "nb" = list(
      type="classification",
      name = "naiveBayes"
    )
  )
  algor.names <- c("max", "knn", "svm", "cart", "nn", "nb")
  algor.pairs <- t(combn(algor.names, 2))
  nparis <- nrow(algor.pairs)
  all.exprs <- c()
  for(data.name in data.names) {
    all.exprs <- rbind(all.exprs, cbind(rep(data.name, nparis), algor.pairs))
  }
  n.all.exprs <- nrow(all.exprs)
  # 开始真实值的测试。
  fold.cnt <- n.all.exprs %/% 10
  start.idx <- (fold-1)*fold.cnt+1
  end.idx <- fold*fold.cnt
  if(fold == 10)
    end.idx <- n.all.exprs
  source("./tasks/true_performance_simulation.R", encoding="UTF-8")
  results <- c()
  for(idx in seq(start.idx, end.idx)) {
    a.expr <- all.exprs[idx,]
    data.name <- a.expr[1]
    algor1.name <- a.expr[2]
    algor2.name <- a.expr[3]
    dataset.conf <- list(
      type="classification",
      name=data.name,
      omit_na = T
    )
    algor1.conf <- algorithms.configs[[algor1.name]]
    algor2.conf <- algorithms.configs[[algor2.name]]
    conf_TASK_TRUECI <- list (
      alpha = 0.05,
      rep = 1000,
      close_form = TRUE,
      n1 = -1,
      prop = 0.5,
      dataset.conf = dataset.conf,
      algorithm1.conf = algor1.conf,
      algorithm2.conf = algor2.conf,
      seq = F,
      multicore = TRUE,
      cores = 20
    )
    task_config <- true_performance_simulation.validate_config(conf_TASK_TRUECI)
    result <- true_performance_simulation.perform_task(task_config)
    result.vec <- c(result[[1]], result[[2]], result[[3]])
    results <- rbind(results, result.vec)
  }
  all.results <- cbind(all.exprs[start.idx:end.idx,], results)
  write.csv(all.results, file=paste("test_result/true_perf_uci_datasets",  fold, "csv", sep="."))
}


testStateOnUCIDataSets <- function() {
  uci.datanames <- c("artificial","hayesroth","pima",
                     "heart","balance","hepatitis","promoters","bupa",
                     "satellite47","car","iris","spambase","cmc","ironosphere","spect","credit",
                     "krvskp","tae","cylinder","letter","tic_tac_toe","dermatology","magic","titanic",
                     "donors","mammographic","transfusion","ecoli","monk","vehicle","flags","nursery",
                     "vote","flare","optdigits","wave","german","page_block","wine","glass","parkinsons",
                     "yeast","haberman","pendigits","zoo")
  source("./datasets/data_loader.R", encoding="UTF-8")
  data.names <- paste("uci", uci.datanames, sep="_")
  for(data.name in data.names) {
    print(data.name)
    dataset.conf <- list(type="classification", name=data.name, omit_na=T)
    data.obj <- LoadDataSetGenerator(dataset.conf)
    data.func <- data.obj[[1]]
    data.set <- data.func(dataset.conf)
  }
}