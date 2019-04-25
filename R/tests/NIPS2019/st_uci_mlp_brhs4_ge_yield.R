source("datasets/data_loader.R", encoding="UTF-8")
source("mlAlgorithms/algor_loader.R", encoding="UTF-8")
source("crossvalidations/cv_loader.R", encoding="UTF-8")
source("varianceEstimator/var_est_loader.R", encoding = "UTF-8")
source("model_train_and_predict.R", encoding="UTF-8")
source("utils.R", encoding="UTF-8")
source("metrics/metric_loader.R", encoding="UTF-8")

datasets.configs <- list(
  iris = list(
    name = "uci_iris",
    type = "classification"
  ),
  vowel = list(
    name = "uci_vowel",
    type = "classification"
  ),
  seed = list(
    name = "uci_seed",
    type = "classification"
  ),
  heart = list(
    name = "uci_heart",
    type = "classification"
  ),
  balance = list(
    name = "uci_balance",
    type = "classification"
  )
)


task_config <- list(
  # 数据集配置根据用户参数进行自动填充。
  algorithm2.conf = list(
    name = "nnet",
    type = "classification",
    h_size = 0
  ),
  algorithm1.conf = list (
    name = "nnet",
    type = "classification",
    h_size = 0
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
  cvConf    <- task_config$crossvalidation.conf
  metricConf<- task_config$metric.conf
  DataInfo <- LoadDataSetGenerator(dataConf)
  DataGenerator <- DataInfo[[1]]
  DataPackages <- DataInfo[[2]]
  # 生成要检验的第一个算法
  AlgorInfo <- LoadAlgorithmGenerator(algorConf)
  AlgorGenerator <- AlgorInfo[[1]]
  AlgorPackages <- AlgorInfo[[2]]
  # 生成要检验的第二个算法
  AlgorInfo2 <- LoadAlgorithmGenerator(algorConf2)
  AlgorGenerator2 <- AlgorInfo2[[1]]
  AlgorPackages2 <- AlgorInfo2[[2]]
  metric.entry <- LoadPerformanceMetricGenerator(metricConf)[[1]]
  # 生成要使用的交叉验证
  CrossValidationInfo <- LoadCrossValidationGenerator(crossvalidation.name = cvConf$name)
  CrossValidationGenerator <- CrossValidationInfo[[1]]
  CrossValidationPackages <- CrossValidationInfo[[2]]
  
  WorkerInit(DataPackages)
  WorkerInit(AlgorPackages)
  WorkerInit(AlgorPackages2)
  WorkerInit(CrossValidationPackages)
  cvConf_org <- cvConf
  data <- DataGenerator(dataConf)
  n <- nrow(data)    
  cvConf$data <- data  
  muv1 <- NULL
  muv2 <- NULL
  mu_diff_vec <- NULL
  cvConf <- CrossValidationGenerator(cvConf)
  partition.set <- cvConf$partitions
  cvres1 <- TrainAndTestForCrossValidationEstimatorWithPartitionSet(data, partition.set, AlgorGenerator, algorConf, metric.entry=metric.entry, metric.conf=metricConf)
  cvres2 <- TrainAndTestForCrossValidationEstimatorWithPartitionSet(data, partition.set, AlgorGenerator2, algorConf2, metric.entry=metric.entry, metric.conf=metricConf)
  # 存储两个算法的性能估计值
  muv1 <- c(muv1, cvres1[[2]])
  muv2 <- c(muv2, cvres2[[2]])
  mu_diff_vec <- muv1 - muv2
  mu_diff <- mean(mu_diff_vec)
  return(c(muv1, muv2, mu_diff_vec))
}

ge.estimator.master <- function(data_tag, hsize,diff=F,sim_count=10000){
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
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  work.directory <- getwd()
  results <- foreach(i = 1:sim_count, .combine = rbind, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    result <- ge.estimator.slaver(task_config = task_config)
    result
  }
  stopCluster(cl)
  write.table(results, file = paste("NIPS2019/", paste("uci",data_tag, hsize, diff, "brhs4", format(Sys.time(), "%Y%m%d%H%M%S"),sep="_"), sep=""))
}
