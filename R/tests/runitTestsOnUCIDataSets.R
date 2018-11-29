testAllConfigs <- function() {
  assign("last.warning", NULL, envir = baseenv())
  uci.datanames <- c("artificial","hayesroth","pima","australian",
                     "heart","balance","hepatitis","promoters","bupa","horse",
                     "satellite47","car","iris","spambase","cmc","ironosphere","spect","credit",
                     "krvskp","tae","cylinder","letter","tic_tac_toe","dermatology","magic","titanic",
                     "donors","mammographic","transfusion","ecoli","monk","vehicle","flags","nursery",
                     "vote","flare","optdigits","wave","german","page_block","wine","glass","parkinsons",
                     "yeast","haberman","pendigits","zoo")
  uci.datanames.remove <- c("post_operative", "ecoli")
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
  source("./utils.R", encoding="UTF-8")
  source("./datasets/data_loader.R", encoding="UTF-8")
  source("./crossvalidations/cv_loader.R", encoding="UTF-8")
  source("./mlAlgorithms/algor_loader.R", encoding="UTF-8")
  source("./model_train_and_predict.R", encoding="UTF-8")
  for(data.name in data.names) {
    if (data.name != paste("uci", "zoo", sep="_")) 
      next()
    dataset.conf <- list(
      type="classification",
      name=data.name,
      omit_na = T
    )
    crossvalidation.conf <- list(
      name = "hold_out",
      prop = 0.5
    )
    # 加载数据集
    dataset.obj <- LoadDataSetGenerator(dataset.conf)
    dataset.pkg <- dataset.obj[[2]]
    dataset.func <- dataset.obj[[1]]
    WorkerInit(dataset.pkg)
    data.set <- dataset.func(dataset.conf)
    # 加载交叉验证算法
    crossvalidation.conf$data <- data.set
    cv.obj <- LoadCrossValidationGenerator(crossvalidation.name = crossvalidation.conf$name)
    cv.func <- cv.obj[[1]]
    partition.set <- cv.func(crossvalidation.conf)
    for(algor.name in algor.names) {
      assign("last.warning", NULL, envir = baseenv())
      algorithm.conf <- algorithms.configs[[algor.name]]
      algor.obj <- LoadAlgorithmGenerator(algorithm.conf)
      algorithm.entry <- algor.obj[[1]]
      algor.pkg <- algor.obj[[2]]
      WorkerInit(algor.pkg)
      # 训练和测试数据集
      results <- TrainAndTestForCrossValidationEstimatorWithPartitionSet(data.set, partition.set, algorithm.entry, algorithm.conf)
      print(paste(data.name, algor.name, results[[1]], results[[2]]))
      warnings()
    }
  }
}