source("datasets/data_loader.R", encoding="UTF-8")
source("mlAlgorithms/algor_loader.R", encoding="UTF-8")
source("crossvalidations/cv_loader.R", encoding="UTF-8")
source("varianceEstimator/var_est_loader.R", encoding = "UTF-8")
source("model_train_and_predict.R", encoding="UTF-8")
source("utils.R", encoding="UTF-8")
source("metrics/metric_loader.R", encoding="UTF-8")

datasets.configs <- list(
  balance = list(
    name = "uci_balance",
    type = "classification"
  ),
  diabetes = list(
    name = "uci_diabetes",
    type = "classification"
  ),
  glass = list(
    name = "uci_glass",
    type = "classification"
  ),
  heart = list(
    name = "uci_heart",
    type = "classification"
  ),
  ionosphere = list(
    name = "uci_ionosphere",
    type = "classification"
  ),
  iris = list(
    name = "uci_iris",
    type = "classification"
  ),
  vehicle = list(
    name = "uci_vehicle",
    type = "classification"
  ),
  wine = list(
    name = "uci_wine",
    type = "classification"
  ),
  yeast = list(
    name = "uci_yeast",
    type = "classification"
  ),
  seed = list(
    name = "uci_seed",
    type = "classification"
  )
)


task_config <- list(
  # 数据集配置根据用户参数进行自动填充。
  algorithm3.conf = list(
    name = "regressionTree",
    type = "classification",
    test_method = "class"
  ),
  algorithm2.conf = list(
    name = "LinearDiscriminantAnalysis",
    type = "classification"
  ),
  algorithm1.conf = list (
    name = "naiveBayes",
    type = "classification"
  ),
  crossvalidation.conf = list(
    name = "rhsbcv",
    J  = 4
  )
)

# STEP 1: 给定数据集，得到BRHS估计。
ge.estimator.slaver <- function(task_config) {
  dataConf  <- task_config$dataset.conf
  algorConf <- task_config$algorithm1.conf
  algorConf2<- task_config$algorithm2.conf
  algorConf3<- task_config$algorithm3.conf
  cvConf    <- task_config$crossvalidation.conf
  metricConf<- task_config$metric.conf
  DataInfo  <- LoadDataSetGenerator(dataConf)
  DataGenerator <- DataInfo[[1]]
  DataPackages  <- DataInfo[[2]]
  # 配置算法
  AlgorInfo      <- LoadAlgorithmGenerator(algorConf)
  AlgorGenerator <- AlgorInfo[[1]]
  AlgorPackages  <- AlgorInfo[[2]]
  AlgorInfo2     <- LoadAlgorithmGenerator(algorConf2)
  AlgorGenerator2<- AlgorInfo2[[1]]
  AlgorPackages2 <- AlgorInfo2[[2]]
  AlgorInfo3     <- LoadAlgorithmGenerator(algorConf3)
  AlgorGenerator3<- AlgorInfo3[[1]]
  AlgorPackages3 <- AlgorInfo3[[2]]
  # 配置算法性能指标
  metric.entry <- LoadPerformanceMetricGenerator(metricConf)[[1]]
  # 生成要使用的交叉验证
  CrossValidationInfo <- LoadCrossValidationGenerator(crossvalidation.name = cvConf$name)
  CrossValidationGenerator <- CrossValidationInfo[[1]]
  CrossValidationPackages <- CrossValidationInfo[[2]]
  # 初始化必要的R包
  WorkerInit(DataPackages)
  WorkerInit(AlgorPackages)
  WorkerInit(AlgorPackages2)
  WorkerInit(AlgorPackages3)
  WorkerInit(CrossValidationPackages)
  # 生成数据、交叉验证切分信息。
  data <- DataGenerator(dataConf)
  n <- nrow(data)    
  cvConf$data <- data  
  cvConf <- CrossValidationGenerator(cvConf)
  partition.set <- cvConf$partitions
  muv1 <- NULL
  muv2 <- NULL
  muv3 <- NULL
  cvres1 <- TrainAndTestForCrossValidationEstimatorWithPartitionSet(data, partition.set, AlgorGenerator,  algorConf,  metric.entry=metric.entry, metric.conf=metricConf)
  cvres2 <- TrainAndTestForCrossValidationEstimatorWithPartitionSet(data, partition.set, AlgorGenerator2, algorConf2, metric.entry=metric.entry, metric.conf=metricConf)
  cvres3 <- TrainAndTestForCrossValidationEstimatorWithPartitionSet(data, partition.set, AlgorGenerator3, algorConf3, metric.entry=metric.entry, metric.conf=metricConf)
  # 存储两个算法的性能估计值
  muv1 <- c(muv1, cvres1[[2]])
  muv2 <- c(muv2, cvres2[[2]])
  muv3 <- c(muv3, cvres3[[2]])
  return(c(muv1, muv2, muv3))
}

ge.estimator.master <- function(data_tag, sim_count=100){
  library(foreach)
  library(doParallel)
  datasetconfig <- datasets.configs[[data_tag]]
  if(is.null(datasetconfig)) stop("No exists data tag")
  task_config$dataset.conf <- datasetconfig
  if(diff==T)  task_config$algorithm1.conf$h_size <- hsize
  task_config$algorithm2.conf$h_size <- hsize
  task_config$dataset.conf <- ValidateAndResolveDataSetConfiguration(task_config$dataset.conf)
  task_config$algorithm1.conf <- ValidateAndResolveAlgorithmConfiguration(task_config$algorithm1.conf)
  task_config$algorithm2.conf <- ValidateAndResolveAlgorithmConfiguration(task_config$algorithm2.conf)
  task_config$algorithm3.conf <- ValidateAndResolveAlgorithmConfiguration(task_config$algorithm3.conf)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  work.directory <- getwd()
  results <- foreach(i = 1:sim_count, .combine = rbind, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    result <- ge.estimator.slaver(task_config = task_config)
    result
  }
  stopCluster(cl)
  write.table(results, file = paste("NIPS2019/", paste("rep_uci",data_tag, "3x2bcv", format(Sys.time(), "%Y%m%d%H%M%S"),sep="_"), sep=""))
}


