# 为了便于序贯检验的分析工作，我们将各个数据的值跑出来并存储。
source("datasets/data_loader.R", encoding="UTF-8")
source("mlAlgorithms/algor_loader.R", encoding="UTF-8")
source("crossvalidations/cv_loader.R", encoding="UTF-8")
source("varianceEstimator/var_est_loader.R", encoding = "UTF-8")
source("model_train_and_predict.R", encoding="UTF-8")
source("utils.R", encoding="UTF-8")
source("metrics/metric_loader.R", encoding="UTF-8")

dataset.configs <- function(dataset.tag, param, n.size) {
  config.letter.1 <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "uci_letter",
      type = "classification",
      samplingConf = list(
        n = 300
      )
    ),
    algorithm1.conf = list(
      name = "regressionTree",
      type = "classification",
      test_method = "class"
    ),
    algorithm2.conf = list(
      name = "firstNearestNeighborhood",
      type = "classification",
      w = 1
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.letter.2 <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "uci_letter",
      type = "classification",
      samplingConf = list(
        n = 300
      )
    ),
    algorithm1.conf = list(
      name = "regressionTree",
      type = "classification",
      test_method = "class"
    ),
    algorithm2.conf = list(
      name = "firstNearestNeighborhood",
      type = "classification",
      w = 5
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.letter.3 <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "uci_letter",
      type = "classification",
      samplingConf = list(
        n = 300
      )
    ),
    algorithm1.conf = list(
      name = "regressionTree",
      type = "classification",
      test_method = "class"
    ),
    algorithm2.conf = list(
      name = "firstNearestNeighborhood",
      type = "classification",
      w = 10
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.letter.4 <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "uci_letter",
      type = "classification",
      samplingConf = list(
        n = 300
      )
    ),
    algorithm1.conf = list(
      name = "regressionTree",
      type = "classification",
      test_method = "class"
    ),
    algorithm2.conf = list(
      name = "firstNearestNeighborhood",
      type = "classification",
      w = 17.25
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.letter.5 <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "uci_letter",
      type = "classification",
      samplingConf = list(
        n = 300
      )
    ),
    algorithm1.conf = list(
      name = "regressionTree",
      type = "classification",
      test_method = "class"
    ),
    algorithm2.conf = list(
      name = "firstNearestNeighborhood",
      type = "classification",
      w = 25
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.letter.6 <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "uci_letter",
      type = "classification",
      samplingConf = list(
        n = 300
      )
    ),
    algorithm1.conf = list(
      name = "regressionTree",
      type = "classification",
      test_method = "class"
    ),
    algorithm2.conf = list(
      name = "firstNearestNeighborhood",
      type = "classification",
      w = 2048
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.scla.first <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim1"
    ),
    algorithm2.conf = list(
      name = "regressionTree",
      type = "classification",
      test_method = "regression"
    ),
    algorithm1.conf = list (
      name = "linearRegrClassifier",
      type = "classification"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.scla.second <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim2"
    ),
    algorithm2.conf = list(
      name = "regressionTree",
      type = "classification",
      test_method = "regression"
    ),
    algorithm1.conf = list (
      name = "linearRegrClassifier",
      type = "classification"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.scla.third <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim3"
    ),
    algorithm2.conf = list(
      name = "regressionTree",
      type = "classification",
      test_method = "regression"
    ),
    algorithm1.conf = list (
      name = "linearRegrClassifier",
      type = "classification"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.scla.fourth <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim4"
    ),
    algorithm2.conf = list(
      name = "regressionTree",
      type = "classification",
      test_method = "regression"
    ),
    algorithm1.conf = list (
      name = "linearRegrClassifier",
      type = "classification"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.sreg.first <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "SimpleNormalDataSet",
      type = "regression",
      shortcut_name = "infor_for_gene_err_sim1"
    ),algorithm2.conf = list(
      name = "linearModel",
      type = "regression",
      no_intercept = FALSE
    ),
    algorithm1.conf = list (
      name = "simpleMeanRegression",
      type = "regression"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.sreg.second <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "SimpleNormalDataSet",
      type = "regression",
      shortcut_name = "infor_for_gene_err_sim2"
    ),algorithm2.conf = list(
      name = "linearModel",
      type = "regression",
      no_intercept = FALSE
    ),
    algorithm1.conf = list (
      name = "simpleMeanRegression",
      type = "regression"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.sreg.third <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "SimpleNormalDataSet",
      type = "regression",
      shortcut_name = "infor_for_gene_err"
    ),algorithm2.conf = list(
      name = "linearModel",
      type = "regression",
      no_intercept = FALSE
    ),
    algorithm1.conf = list (
      name = "simpleMeanRegression",
      type = "regression"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.sreg.fourth <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "SimpleNormalDataSet",
      type = "regression",
      shortcut_name = "infor_for_gene_err_sim4"
    ),
    algorithm2.conf = list(
      name = "linearModel",
      type = "regression",
      no_intercept = FALSE
    ),
    algorithm1.conf = list (
      name = "simpleMeanRegression",
      type = "regression"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.sreg <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "SimpleNormalDataSet",
      type = "regression",
      shortcut_name = "infor_for_gene_err",
      mu_scale_y = param,
      n = n.size
    ),
    algorithm2.conf = list(
      name = "linearModel",
      type = "regression",
      no_intercept = FALSE
    ),
    algorithm1.conf = list (
      name = "simpleMeanRegression",
      type = "regression"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.sreg.kknn <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "SimpleNormalDataSet",
      type = "regression",
      shortcut_name = "infor_for_gene_err",
      mu_scale_y = param,
      n = n.size
    ),
    algorithm2.conf = list(
      name = "kknn",
      type = "regression"
    ),
    algorithm1.conf = list (
      name = "simpleMeanRegression",
      type = "regression"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.markatou05regr <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "markatou05regr",
      type = "regression",
      nsize = n.size
    ),
    algorithm2.conf = list(
      name = "linearModel",
      type = "regression",
      no_intercept = FALSE
    ),
    algorithm1.conf = list (
      name = "simpleMeanRegression",
      type = "regression"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.scla <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim3",
      mu1 = rep(param, 2)
    ),
    algorithm2.conf = list(
      name = "classificationTree",
      type = "classification",
      method = "class"
    ),
    algorithm1.conf = list (
      name = "linearRegrClassifier",
      type = "classification"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.letter <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "uci_letter",
      type = "classification",
      binarize = TRUE,
      samplingConf = list(
        n = n.size
      )
    ),
    algorithm2.conf = list(
      name = "classificationTree",
      type = "classification",
      method = "class"
    ),
    algorithm1.conf = list(
      name = "firstNearestNeighborhood",
      type = "classification",
      w = param
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.sreg1 <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "SimpleNormalDataSet",
      type = "regression",
      shortcut_name = "infor_for_gene_err_sim1",
      mu_scale_y = param,
      n = n.size
    ),
    algorithm2.conf = list(
      name = "linearModel",
      type = "regression",
      no_intercept = FALSE
    ),
    algorithm1.conf = list (
      name = "simpleMeanRegression",
      type = "regression"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.sreg.var1 <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "SimpleNormalDataSet",
      type = "regression",
      shortcut_name = "infor_for_gene_err",
      mu_scale_y = param,
      sigma_y = param * 9.97 /0.1,
      n = n.size
    ),
    algorithm2.conf = list(
      name = "linearModel",
      type = "regression",
      no_intercept = FALSE
    ),
    algorithm1.conf = list (
      name = "simpleMeanRegression",
      type = "regression"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.sreg.var2 <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "SimpleNormalDataSet",
      type = "regression",
      shortcut_name = "infor_for_gene_err",
      mu_scale_y = param,
      n = n.size
    ),
    algorithm2.conf = list(
      name = "linearModel",
      type = "regression",
      no_intercept = FALSE
    ),
    algorithm1.conf = list (
      name = "rtreeModel",
      type = "regression"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.sreg.largevar <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "SimpleNormalDataSet",
      type = "regression",
      shortcut_name = "infor_for_gene_err",
      mu_scale_y = param,
      n = n.size,
      sigma_y = 20
    ),
    algorithm2.conf = list(
      name = "linearModel",
      type = "regression",
      no_intercept = FALSE
    ),
    algorithm1.conf = list (
      name = "simpleMeanRegression",
      type = "regression"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.sreg.largevar2 <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "SimpleNormalDataSet",
      type = "regression",
      shortcut_name = "infor_for_gene_err",
      mu_scale_y = param,
      n = n.size,
      sigma_x = 10,
      sigma_y = 20
    ),
    algorithm2.conf = list(
      name = "linearModel",
      type = "regression",
      no_intercept = FALSE
    ),
    algorithm1.conf = list (
      name = "simpleMeanRegression",
      type = "regression"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  config.sreg.ridge <- list(
    lower_m = 3,
    upper_m = 20,
    dataset.conf = list(
      name = "SimpleNormalDataSet",
      type = "regression",
      shortcut_name = "infor_for_gene_err",
      mu_scale_y = param,
      n = n.size,
      sigma_x = 10,
      sigma_y = 20
    ),
    algorithm2.conf = list(
      name = "lmRidgeModel",
      type = "regression"
      
    ),
    algorithm1.conf = list (
      name = "simpleMeanRegression",
      type = "regression"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  
  if(dataset.tag == "sreg.var1")
    return(config.sreg.var1)
  if(dataset.tag == "sreg")
    return(config.sreg)
  if(dataset.tag == "scla" )
    return(config.scla)
  if(dataset.tag == "letter")
    return(config.letter)
  if(dataset.tag == "sreg1")
    return(config.sreg1)
  if(dataset.tag == "sreg.lv")
    return(config.sreg.largevar)
  if(dataset.tag == "sreg.lv2")
    return(config.sreg.largevar2)
  if(dataset.tag == "sreg.ridge")
    return(config.sreg.ridge)
  if(dataset.tag == "markatou")
    return(config.markatou05regr)
  if(dataset.tag == "sreg.kknn")
    return(config.sreg.kknn)
  if(dataset.tag == "sreg.fourth")
    return(config.sreg.fourth)
  if(dataset.tag == "sreg.first")
    return(config.sreg.first)
  if(dataset.tag == "sreg.second")
    return(config.sreg.second)
  if(dataset.tag == "sreg.third")
    return(config.sreg.third)
  if(dataset.tag == "scla.first")
    return(config.scla.first)
  if(dataset.tag == "scla.second")
    return(config.scla.second)
  if(dataset.tag == "scla.third")
    return(config.scla.third)
  if(dataset.tag == "scla.fourth")
    return(config.scla.fourth)
  if(dataset.tag == "letter.1")
    return(config.letter.1)
  if(dataset.tag == "letter.2")
    return(config.letter.2)
  if(dataset.tag == "letter.3")
    return(config.letter.3)
  if(dataset.tag == "letter.4")
    return(config.letter.4)
  if(dataset.tag == "letter.5")
    return(config.letter.5)
  if(dataset.tag == "letter.6")
    return(config.letter.6)
  return(NULL)
}

collect.data.set <- function(dataset.tag, param, n.size, task_config) {
  dataConf  <- task_config$dataset.conf
  algorConf <- task_config$algorithm1.conf
  algorConf2<- task_config$algorithm2.conf
  cvConf    <- task_config$crossvalidation.conf
  metricConf<- task_config$metric.conf
  upper_m   <- task_config$upper_m  # m参数停止的上界
  lower_m   <- 3  # m参数的起始值
  relative  <- task_config$relative # 相对误差
  if(!is.null(task_config$lower_m)) {
    lower_m <- task_config$lower_m 
  }
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
  # 交叉验证类型必须为增量式mx2bcv.
  if(cvConf$name != 'mx2bcv_inc' && cvConf$name != 'mxkrcv_inc') {
    stop("cross validation needed: increaseBalancedMx2cv || mxkrcv_inc");
  }
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
  cur_m <- lower_m  # 存储当前的m值 
  muv1 <- NULL
  muv2 <- NULL
  mu_diff_vec <- NULL
  for(cur_m in lower_m:upper_m) {
    # 计算机器学习算法的性能估计
    cvConf$m <- cur_m    
    cvConf <- CrossValidationGenerator(cvConf)
    partition.set <- cvConf$splits_new
    cvres1 <- TrainAndTestForCrossValidationEstimatorWithPartitionSet(data, partition.set, AlgorGenerator, algorConf, metric.entry=metric.entry, metric.conf=metricConf)
    cvres2 <- TrainAndTestForCrossValidationEstimatorWithPartitionSet(data, partition.set, AlgorGenerator2, algorConf2, metric.entry=metric.entry, metric.conf=metricConf)
    # 存储两个算法的性能估计值
    muv1 <- c(muv1, cvres1[[2]])
    muv2 <- c(muv2, cvres2[[2]])
    mu_diff_vec <- muv1 - muv2
    if(!is.null(relative) && relative == T) {
      mu_diff_vec <- mu_diff_vec / muv1
    }
    mu_diff <- mean(mu_diff_vec)
  }
  return(c(muv1, muv2, mu_diff_vec))
}


collect.data.set.entry <- function(dataset.tag, param, n.size=NULL, sim_count=10000){
  library(foreach)
  library(doParallel)
  task_config <- dataset.configs(dataset.tag, param, n.size)
  task_config$dataset.conf <- ValidateAndResolveDataSetConfiguration(task_config$dataset.conf)
  task_config$algorithm1.conf <- ValidateAndResolveAlgorithmConfiguration(task_config$algorithm1.conf)
  task_config$algorithm2.conf <- ValidateAndResolveAlgorithmConfiguration(task_config$algorithm2.conf)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  work.directory <- getwd()
  results <- foreach(i = 1:sim_count, .combine = rbind, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    set.seed(i+123)
    result <- collect.data.set(dataset.tag=dataset.tag, param=param, n.size=n.size, task_config = task_config)
    result
  }
  stopCluster(cl)
  n.str <- 'default'
  if(!is.null(n.size)) {
    n.str <- n.size
  }
  write.table(results, file = paste("est_for_test/", paste(dataset.tag, param, n.size, sim_count,format(Sys.time(), "%Y%m%d%H%M%S"),sep="_"), sep=""))
}

collect.data.set.entry.seq <- function(dataset.tag, param, n.size=NULL, sim_count=1000){
  task_config <- dataset.configs(dataset.tag, param, n.size)
  task_config$dataset.conf <- ValidateAndResolveDataSetConfiguration(task_config$dataset.conf)
  task_config$algorithm1.conf <- ValidateAndResolveAlgorithmConfiguration(task_config$algorithm1.conf)
  task_config$algorithm2.conf <- ValidateAndResolveAlgorithmConfiguration(task_config$algorithm2.conf)
  results <- c()
  for(i in 1:sim_count) {
    result <- collect.data.set(dataset.tag=dataset.tag, param=param, n.size=n.size, task_config = task_config)    
    results <- rbind(results, result)
  }
  return(results)
}

# 给定mx2交叉验证的估计矩阵，计算出估计的均值，及相关系数。
computeStatisticForEstMatrix <- function(est.matrix) {
  mean.value <- mean(est.matrix)
  cor.matrix.A <- cor(est.matrix)
  #计算rho_1
  n.col.per.unit <- ncol(est.matrix)
  rho_1_vec <- c()
  for(j in 1:(n.col.per.unit/2)) {
    rho_1_vec <- c(rho_1_vec, cor.matrix.A[2*j-1, 2*j])
    rho_1_vec <- c(rho_1_vec, cor.matrix.A[2*j, 2*j-1])
  }
  rho.1 <- mean(rho_1_vec)
  #计算rho_2
  for(j in 1:(n.col.per.unit/2)) {
    cor.matrix.A[2*j-1, 2*j] <- NA
    cor.matrix.A[2*j, 2*j-1] <- NA
  }
  diag(cor.matrix.A) <- NA
  rho.2 <- mean(cor.matrix.A, na.rm = T)
  return(c(mean.value, rho.1, rho.2))
}


# 对于给定数据集，计算算法A、算法B以及算法A和B的性能差异的rho_1, rho_2以及性能的置信区间。
statEstMatrixInfo <- function(file.name) {
  est.data <-  as.matrix(read.table(file.name))
  n.col.est.data <- ncol(est.data)
  if(n.col.est.data %% 3 != 0) stop("invalid format of provided data file.")
  n.col.per.unit <- n.col.est.data/3
  # 算法A
  est.data.A <- est.data[,1:n.col.per.unit]
  stat.A <- computeStatisticForEstMatrix(est.data.A)
  # 算法B
  est.data.B <- est.data[,(n.col.per.unit+1):(2*n.col.per.unit)]
  stat.B <- computeStatisticForEstMatrix(est.data.B)
  # 算法B-A
  est.data.diff <- est.data[,(2*n.col.per.unit+1):(3*n.col.per.unit)]
  stat.diff <- computeStatisticForEstMatrix(est.data.diff)
  return(c(stat.A, stat.B, stat.diff))
}


testStatDataSetInfo <- function() {
  # SReg数据
  file.name.sreg1 <- "C:/Users/YJ/Desktop/a/result/sreg.first_-1_-1_1000_20180618115602"
  file.name.sreg2 <- "C:/Users/YJ/Desktop/a/result/sreg.second_-1_-1_10000_20180625161459"
  file.name.sreg3 <- "C:/Users/YJ/Desktop/a/result/sreg.third_-1_-1_1000_20180618115611"
  file.name.sreg4 <- "C:/Users/YJ/Desktop/a/result/sreg.fourth_-1_-1_1000_20180618115618"
  stat.info.reg1 <- statEstMatrixInfo(file.name.sreg1)
  stat.info.reg2 <- statEstMatrixInfo(file.name.sreg2)
  stat.info.reg3 <- statEstMatrixInfo(file.name.sreg3)
  stat.info.reg4 <- statEstMatrixInfo(file.name.sreg4)
  sreg.stat <- rbind(stat.info.reg1, stat.info.reg2, stat.info.reg3, stat.info.reg4)
  # SCla 数据
  file.name.scla1 <- "C:/Users/YJ/Desktop/a/result/scla.first_-1_-1_1000_20180618115622"
  file.name.scla2 <- "C:/Users/YJ/Desktop/a/result/scla.second_-1_-1_1000_20180618115628"
  file.name.scla3 <- "C:/Users/YJ/Desktop/a/result/scla.third_-1_-1_1000_20180618115630"
  file.name.scla4 <- "C:/Users/YJ/Desktop/a/result/scla.fourth_-1_-1_1000_20180618115632"
  stat.info.cla1 <- statEstMatrixInfo(file.name.scla1)
  stat.info.cla2 <- statEstMatrixInfo(file.name.scla2)
  stat.info.cla3 <- statEstMatrixInfo(file.name.scla3)
  stat.info.cla4 <- statEstMatrixInfo(file.name.scla4)
  scla.stat <- rbind(stat.info.cla1, stat.info.cla2, stat.info.cla3, stat.info.cla4)
  # Letter 数据
  #file.name.letter1 <- "C:/Users/YJ/Desktop/a/result/letter.1_-1_-1_1000_20180618115653"
  #file.name.letter2 <- "C:/Users/YJ/Desktop/a/result/letter.2_-1_-1_1000_20180618115655"
  #file.name.letter3 <- "C:/Users/YJ/Desktop/a/result/letter.3_-1_-1_1000_20180618115658"
  #file.name.letter4 <- "C:/Users/YJ/Desktop/a/result/letter.4_-1_-1_1000_20180618115658"
  #file.name.letter5 <- "C:/Users/YJ/Desktop/a/result/letter.5_-1_-1_1000_20180618115700"
  #file.name.letter6 <- "C:/Users/YJ/Desktop/a/result/letter.6_-1_-1_1000_20180618115706"
  file.name.letter1 <- "C:/Users/YJ/Desktop/a/result/letter.1_-1_-1_1000_20181107223023"
  file.name.letter2 <- "C:/Users/YJ/Desktop/a/result/letter.2_-1_-1_1000_20181107223542"
  file.name.letter3 <- "C:/Users/YJ/Desktop/a/result/letter.3_-1_-1_1000_20181107223548"
  file.name.letter4 <- "C:/Users/YJ/Desktop/a/result/letter.4_-1_-1_1000_20181107223745"
  file.name.letter5 <- "C:/Users/YJ/Desktop/a/result/letter.5_-1_-1_1000_20181107223333"
  file.name.letter6 <- "C:/Users/YJ/Desktop/a/result/letter.6_-1_-1_1000_20181107223629"
  stat.info.letter1 <- statEstMatrixInfo(file.name.letter1)
  stat.info.letter2 <- statEstMatrixInfo(file.name.letter2)
  stat.info.letter3 <- statEstMatrixInfo(file.name.letter3)
  stat.info.letter4 <- statEstMatrixInfo(file.name.letter4)
  stat.info.letter5 <- statEstMatrixInfo(file.name.letter5)
  stat.info.letter6 <- statEstMatrixInfo(file.name.letter6)
  letter.stat <- rbind(stat.info.letter1, stat.info.letter2, stat.info.letter3, stat.info.letter4, stat.info.letter5, stat.info.letter6)
  print(sreg.stat)
  print(scla.stat)
  print(letter.stat)
}


formatMatrix <- function (vec) {
  n.m.expect <- length(vec)
  n.row <- (sqrt(8*n.m.expect+1)-1)/2
  expect.matrix <- matrix(rep(NA, n.row^2), n.row)
  index <- 1
  for(i in 1:n.row) {
    for(j in 1:i) {
      expect.matrix[i,j] <- vec[index]
      index <- index+1
    }
  }
  return(expect.matrix)
}

computeExpectStopTime <- function(m.h0, m.h1, m.no, repi = 1000) {
  m.no[length(m.no)] <- (repi - (sum(m.h0)+sum(m.h1)))
  m.dec <- m.h0+m.h1+m.no
  freq.v <- m.dec /repi
  count.vec <- 3:(3+length(m.h0)-1)
  m.expect <- round(sum(freq.v %*% count.vec))
  return(m.expect)
}

computeExpectStopTimeForTestResult <- function(file.name, repi = 1000) {
  test.result <- read.csv(file.name)
  n.result <- ncol(test.result)
  result.m <- test.result[, 4:(n.result-5)]
  n.col <- ncol(result.m)
  n.unit <- n.col /3
  m.h0 <- result.m[,1:n.unit]
  m.h1 <- result.m[,(n.unit+1):(2*n.unit)]
  m.no <- result.m[,(2*n.unit+1):(3*n.unit)]
  m.no[, ncol(m.no)] <- (repi - (rowSums(m.h0)+rowSums(m.h1)))
  m.dec <- m.h0+m.h1+m.no
  freq.v <- m.dec /repi
  count.vec <- 3:(3+n.unit-1)
  m.expect <- round(rowSums(as.matrix(freq.v) %*% t(t(count.vec))))
  return(m.expect)
}

computeDecisionExpectedStopTimeForTestResult <- function(file.name) {
  test.result <- read.csv(file.name)
  n.result <- ncol(test.result)
  result.m <- test.result[, 4:(n.result-5)]
  n.col <- ncol(result.m)
  n.unit <- n.col /3
  m.h0 <- result.m[,1:n.unit]
  m.h1 <- result.m[,(n.unit+1):(2*n.unit)]
  m.dec <- m.h0+m.h1
  row.sum <- rowSums(m.dec)
  freq.v <- c()
  for(i in 1:length(row.sum)) {
    freq.v <- rbind(freq.v, m.dec[i,]/row.sum[i])
  }
  count.vec <- 3:(3+n.unit-1)
  m.expect <- round(rowSums(as.matrix(freq.v) %*% t(t(count.vec))))
  m.expect[which(is.nan(m.expect))] <- 0
  return(m.expect)
}

computeStopTimeForTestResult <- function(file.name, true.value, loss.vec = list("f"=100, "l"=80, "c"=20)) {
  test.result <- read.csv(file.name)
  delta_0 <- test.result[,2]
  delta_1 <- test.result[,3]
  n.col.data <- ncol(test.result)
  prop.acch0 <- test.result[,n.col.data-4]
  prop.acch1 <- test.result[,n.col.data-3]
  prop.accno <- test.result[,n.col.data-2]
  prop.accno <- 1- prop.acch0- prop.acch1
  error.type1 <- test.result[,n.col.data-1]
  error.type2 <- test.result[,n.col.data-0]
  # 计算m值得期望
  m.expect <- computeExpectStopTimeForTestResult(file.name)
  m.expect.matrix <- formatMatrix(m.expect)
  # 计算风险值
  risk.vec <- rep(0, nrow(test.result))
  loss.fatal <- loss.vec$f
  loss.lib <- loss.vec$l
  loss.cons <- loss.vec$c
  # true value< delta0 应该接受h0.
  risk.1 <- prop.acch1 * loss.fatal + prop.accno * loss.cons
  index.1 <-  which(true.value < delta_0)
  risk.vec[index.1] <- risk.1[index.1]
  # true value > delta1 应该接受h1
  risk.2 <- prop.acch0 * loss.fatal + prop.accno * loss.cons
  index.2 <- which(true.value > delta_1)
  risk.vec[index.2] <- risk.2[index.2]
  # delta0 < true value < delta1 应该不接受任何假设
  risk.3 <- prop.acch0 * loss.lib + prop.acch1 * loss.lib
  index.3 <- which((delta_0 <= true.value ) & (true.value <= delta_1))
  risk.vec[index.3] <- risk.3[index.3]
  risk.matrix <- formatMatrix(risk.vec)
  return(list(m.expect.matrix, risk.matrix))
}
rotateMatrix <- function(m) {
  n.row <- nrow(m)
  n.col <- ncol(m)
  m.new <- matrix(rep(NA, n.row*n.col), nrow = n.row)
  for(i in 1:n.row){
    for(j in 1:n.col){
      m.new[i,j] <- m[n.row-i+1, n.col-j+1]
    }
  }
  return(m.new)  
}
computeTypeErrorsForTestResult <- function(file.name, true.value) {
  test.result <- read.csv(file.name)
  delta_0 <- test.result[,2]
  delta_1 <- test.result[,3]
  n.col.data <- ncol(test.result)
  error.type1 <- test.result[,n.col.data-1]
  error.type2 <- test.result[,n.col.data-0]
  error.type1.matrix <- formatMatrix(error.type1)
  error.type2.matrix <- formatMatrix(error.type2)
  error.type2.matrix.rotate <- rotateMatrix(error.type2.matrix)
  return(list(error.type1.matrix, error.type2.matrix))
}

testFormatTestResult <- function() {
  #sreg1 diet
  file.name.diet.sreg1 <- "C:/Users/YJ/Desktop/a/result/diet_sreg.first_20180818103655"
  result.diet.sreg1 <- computeStopTimeForTestResult(file.name.diet.sreg1, true.value = -0.0255)
  # sreg1 14est
  file.name.14est.sreg1 <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.first_type14est_20180818110220"
  result.14est.sreg1 <- computeStopTimeForTestResult(file.name.14est.sreg1, true.value = -0.0255)
  #sreg1 V
  file.name.V.sreg1 <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.first_typeV_20180818113009"
  result.V.sreg1 <- computeStopTimeForTestResult(file.name.V.sreg1, true.value = -0.0255)
  
  # type error
  file.name.sreg1.diet <- "C:/Users/YJ/Desktop/a/result/diet_sreg.first_TRUE_20180818165805"
  result.err.diet.sreg1 <- computeTypeErrorsForTestResult(file.name.sreg1.diet, -0.0255)
  file.name.sreg1.14est <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.first_type14est_TRUE_20180818165426"
  result.err.14est.sreg1 <- computeTypeErrorsForTestResult(file.name.sreg1.14est, -0.0255)
  file.name.sreg1.V <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.first_typeV_TRUE_20180818170030"
  result.err.V.sreg1 <- computeTypeErrorsForTestResult(file.name.sreg1.V, -0.0255)
  file.name.diet.sreg1 <- "C:/Users/YJ/Desktop/a/result/diet_sreg.first_20180818103655"
  result.dectime.diet.sreg1 <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.diet.sreg1))
  file.name.14est.sreg1 <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.first_type14est_20180818110220"
  result.dectime.14est.sreg1 <-formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.14est.sreg1))
  file.name.V.sreg1 <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.first_typeV_20180818113009"
  result.dectime.V.sreg1 <-formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.V.sreg1))
  
  
  # sreg2 14est
  file.name.diet.sreg2 <- "C:/Users/YJ/Desktop/a/result/diet_sreg.second_20180818103929"
  result.diet.sreg2 <- computeStopTimeForTestResult(file.name.diet.sreg2, true.value = 7.4647)
  file.name.sreg2 <- "C:/Users/YJ/Desktop/a/result/diet_sreg.second_TRUE_20180818165404"
  result.err.diet.sreg2 <- computeTypeErrorsForTestResult(file.name.sreg2, 7.4647)
  file.name.diet.sreg2 <- "C:/Users/YJ/Desktop/a/result/diet_sreg.second_20180818103929"
  result.dectime.diet.sreg2 <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.diet.sreg2))
  print(result.diet.sreg2)
  print(result.err.diet.sreg2)
  print(result.dectime.diet.sreg2)
  
  file.name.14est.sreg2 <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.second_type14est_20180818110412"
  result.14est.sreg2 <- computeStopTimeForTestResult(file.name.14est.sreg2, true.value = 7.4647)
  file.name.sreg2.14est <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.second_type14est_TRUE_20180818165334"
  result.err.14est.sreg2 <- computeTypeErrorsForTestResult(file.name.sreg2.14est, 7.4647)
  file.name.14est.sreg2 <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.second_type14est_20180818110412"
  result.dectime.14est.sreg2 <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.14est.sreg2))
  print(result.14est.sreg2)
  print(result.err.14est.sreg2)
  print(result.dectime.14est.sreg2)
  
  file.name.V.sreg2 <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.second_typeV_20180818112557"
  result.V.sreg2 <- computeStopTimeForTestResult(file.name.V.sreg2, true.value = 7.4647)
  file.name.sreg2.V <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.second_typeV_TRUE_20180818170257"
  result.err.V.sreg2 <- computeTypeErrorsForTestResult(file.name.sreg2.V, 7.4647)
  file.name.V.sreg2 <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.second_typeV_20180818112557"
  result.dectime.V.sreg2 <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.V.sreg2))
  print(result.V.sreg2)
  print(result.err.V.sreg2)
  print(result.dectime.V.sreg2)
  
  
  
  
  file.name.diet.sreg3 <- "C:/Users/YJ/Desktop/a/result/diet_sreg.third_20180818103146"
  result.diet.sreg3 <- computeStopTimeForTestResult(file.name.diet.sreg3, true.value = 0.0011)
  file.name.sreg3 <- "C:/Users/YJ/Desktop/a/result/diet_sreg.third_TRUE_20180818165357"
  result.err.diet.sreg3 <- computeTypeErrorsForTestResult(file.name.sreg3, 0.0011)
  file.name.diet.sreg3 <- "C:/Users/YJ/Desktop/a/result/diet_sreg.third_20180818103146"
  result.dectime.diet.sreg3 <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.diet.sreg3))
  print(result.diet.sreg3)
  print(result.err.diet.sreg3)
  print(result.dectime.diet.sreg3)
  
  file.name.14est.sreg3 <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.third_type14est_20180818105739"
  result.14est.sreg3 <- computeStopTimeForTestResult(file.name.14est.sreg3, true.value = 0.0011)
  file.name.sreg3.14est <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.third_type14est_TRUE_20180818165324"
  result.err.14est.sreg3 <- computeTypeErrorsForTestResult(file.name.sreg3.14est, 0.0011)
  file.name.14est.sreg3 <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.third_type14est_20180818105739"
  result.dectime.14est.sreg3 <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.14est.sreg3))
  print(result.14est.sreg3)
  print(result.err.14est.sreg3)
  print(result.dectime.14est.sreg3)
  
  
  file.name.V.sreg3 <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.third_typeV_20180818112259"
  result.V.sreg3 <- computeStopTimeForTestResult(file.name.V.sreg3, true.value = 0.0011)
  file.name.sreg3.V <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.third_typeV_TRUE_20180818170721"
  result.err.V.sreg3 <- computeTypeErrorsForTestResult(file.name.sreg3.V, 0.0011)
  file.name.V.sreg3 <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.third_typeV_20180818112259"
  result.dectime.V.sreg3 <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.V.sreg3))
  print(result.V.sreg3)
  print(result.err.V.sreg3)
  print(result.dectime.V.sreg3)
  
  
  file.name.diet.sreg4 <- "C:/Users/YJ/Desktop/a/result/diet_sreg.fourth_20180818103149"
  result.diet.sreg4 <- computeStopTimeForTestResult(file.name.diet.sreg4, true.value = 0.0398)
  file.name.sreg4 <- "C:/Users/YJ/Desktop/a/result/diet_sreg.fourth_TRUE_20180818164902"
  result.err.diet.sreg4 <- computeTypeErrorsForTestResult(file.name.sreg4, 0.0398)
  file.name.diet.sreg4 <- "C:/Users/YJ/Desktop/a/result/diet_sreg.fourth_20180818103149"
  result.dectime.diet.sreg4 <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.diet.sreg4))
  print(result.diet.sreg4)
  print(result.err.diet.sreg4)
  print(result.dectime.diet.sreg4)
  
  file.name.14est.sreg4 <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.fourth_type14est_20180818105659"
  result.14est.sreg4 <- computeStopTimeForTestResult(file.name.14est.sreg4, true.value = 0.0398)
  file.name.sreg4.14est <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.fourth_type14est_TRUE_20180818165526"
  result.err.14est.sreg4 <- computeTypeErrorsForTestResult(file.name.sreg4.14est, 0.0398)
  file.name.14est.sreg4 <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.fourth_type14est_20180818105659"
  result.dectime.14est.sreg4 <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.14est.sreg4))
  print(result.14est.sreg4)
  print(result.err.14est.sreg4)
  print(result.dectime.14est.sreg4)
  
  file.name.V.sreg4 <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.fourth_typeV_20180818112406"
  result.V.sreg4 <- computeStopTimeForTestResult(file.name.V.sreg4, true.value = 0.0398)
  file.name.sreg4.V <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.fourth_typeV_TRUE_20180818170347"
  result.err.V.sreg4 <- computeTypeErrorsForTestResult(file.name.sreg4.V, 0.0398)
  file.name.V.sreg4 <- "C:/Users/YJ/Desktop/a/result/bmx2_sreg.fourth_typeV_20180818112406"
  result.dectime.V.sreg4 <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.V.sreg4))
  print(result.V.sreg4)
  print(result.err.V.sreg4)
  print(result.dectime.V.sreg4)
  
  
  file.name.diet.scla1 <- "C:/Users/YJ/Desktop/a/result/diet_scla.first_20180818103127"
  result.risk <- computeStopTimeForTestResult(file.name.diet.scla1, true.value = -0.0423)
  file.name.scla1 <- "C:/Users/YJ/Desktop/a/result/diet_scla.first_TRUE_20180818164932"
  result.err <- computeTypeErrorsForTestResult(file.name.scla1, -0.0423)
  file.name.diet.scla1 <- "C:/Users/YJ/Desktop/a/result/diet_scla.first_20180818103127"
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.diet.scla1))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.14est.scla1 <- "C:/Users/YJ/Desktop/a/result/bmx2_scla.first_type14est_20180818105847"
  result.risk <- computeStopTimeForTestResult(file.name.14est.scla1, true.value = -0.0423)
  file.name.sreg4.14est <- "C:/Users/YJ/Desktop/a/result/bmx2_scla.first_type14est_TRUE_20180818165415"
  result.err <- computeTypeErrorsForTestResult(file.name.sreg4.14est, -0.0423)
  file.name.14est.scla1 <- "C:/Users/YJ/Desktop/a/result/bmx2_scla.first_type14est_20180818105847"
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.14est.scla1))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.V.scla1 <- "C:/Users/YJ/Desktop/a/result/bmx2_scla.first_typeV_20180818112638"
  result.risk <- computeStopTimeForTestResult(file.name.V.scla1, true.value = -0.0423)
  file.name.scla1.V <- "C:/Users/YJ/Desktop/a/result/bmx2_scla.first_typeV_TRUE_20180818170557"
  result.err <- computeTypeErrorsForTestResult(file.name.scla1.V, -0.0423)
  file.name.V.scla1 <- "C:/Users/YJ/Desktop/a/result/bmx2_scla.first_typeV_20180818112638"
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.V.scla1))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.diet.scla2 <- "C:/Users/YJ/Desktop/a/result/diet_scla.second_20180818103254"
  result.risk <- computeStopTimeForTestResult(file.name.diet.scla2, true.value = 0.0012)
  file.name.scla2 <- "C:/Users/YJ/Desktop/a/result/diet_scla.second_TRUE_20180818164936"
  result.err <- computeTypeErrorsForTestResult(file.name.scla2, 0.0012)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.diet.scla2))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.14est.scla2 <- "C:/Users/YJ/Desktop/a/result/bmx2_scla.second_type14est_20180818105926"
  result.risk <- computeStopTimeForTestResult(file.name.14est.scla2, true.value = 0.0012)
  file.name.scla2.14est <- "C:/Users/YJ/Desktop/a/result/bmx2_scla.second_type14est_TRUE_20180818165619"
  result.err <- computeTypeErrorsForTestResult(file.name.scla2.14est, 0.0012)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.14est.scla2))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.V.scla2 <- "C:/Users/YJ/Desktop/a/result/bmx2_scla.second_typeV_20180818112515"
  result.risk <- computeStopTimeForTestResult(file.name.V.scla2, true.value = 0.0012)
  file.name.scla2.V <- "C:/Users/YJ/Desktop/a/result/bmx2_scla.second_typeV_TRUE_20180818170345"
  result.err <- computeTypeErrorsForTestResult(file.name.scla2.V, 0.0012)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.V.scla2))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.diet.scla3 <- "C:/Users/YJ/Desktop/a/result/diet_scla.third_20180818103517"
  result.risk <- computeStopTimeForTestResult(file.name.diet.scla3, true.value = -0.0043)
  file.name.scla3 <- "C:/Users/YJ/Desktop/a/result/diet_scla.third_TRUE_20180818165044"
  result.err <- computeTypeErrorsForTestResult(file.name.scla3, -0.0043)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.diet.scla3))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.14est.scla3 <- "C:/Users/YJ/Desktop/a/result/bmx2_scla.third_type14est_20180818105904"
  result.risk <- computeStopTimeForTestResult(file.name.14est.scla3, true.value = -0.0043)
  file.name.scla3.14est <- "C:/Users/YJ/Desktop/a/result/bmx2_scla.third_type14est_TRUE_20180818165807"
  result.err <- computeTypeErrorsForTestResult(file.name.scla3.14est, -0.0043)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.14est.scla3))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.V.scla3 <- "C:/Users/YJ/Desktop/a/result/bmx2_scla.third_typeV_20180818112513"
  result.risk <- computeStopTimeForTestResult(file.name.V.scla3, true.value = -0.0043)
  file.name.scla3.V <- "C:/Users/YJ/Desktop/a/result/bmx2_scla.third_typeV_TRUE_20180818170731"
  result.err <- computeTypeErrorsForTestResult(file.name.scla3.V, -0.0043)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.V.scla3))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.diet.scla4 <- "C:/Users/YJ/Desktop/a/result/diet_scla.fourth_20180818103120"
  result.risk <- computeStopTimeForTestResult(file.name.diet.scla4, true.value = 0.0240)
  file.name.scla4 <- "C:/Users/YJ/Desktop/a/result/diet_scla.fourth_TRUE_20180818164844"
  result.err <- computeTypeErrorsForTestResult(file.name.scla4, 0.0240)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.diet.scla4))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.14est.scla4 <- "C:/Users/YJ/Desktop/a/result/bmx2_scla.fourth_type14est_20180818110120"
  result.risk <- computeStopTimeForTestResult(file.name.14est.scla4, true.value = 0.0240)
  file.name.scla4.14est <- "C:/Users/YJ/Desktop/a/result/bmx2_scla.fourth_type14est_TRUE_20180818165846"
  result.err <- computeTypeErrorsForTestResult(file.name.scla4.14est, 0.0240)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.14est.scla4))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.V.scla4 <- "C:/Users/YJ/Desktop/a/result/bmx2_scla.fourth_typeV_20180818112543"
  result.risk <- computeStopTimeForTestResult(file.name.V.scla4, true.value = 0.0240)
  file.name.scla4.V <- "C:/Users/YJ/Desktop/a/result/bmx2_scla.fourth_typeV_TRUE_20180818170352"
  result.err <- computeTypeErrorsForTestResult(file.name.scla4.V, 0.0240)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.V.scla4))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  
  file.name.diet.letter.1 <- "C:/Users/YJ/Desktop/a/result/diet_letter.1_20180818103326"
  result.risk <- computeStopTimeForTestResult(file.name.diet.letter.1, true.value = -0.1563)
  file.name.letter1 <- "C:/Users/YJ/Desktop/a/result/diet_letter.1_TRUE_20180818165053"
  result.err <- computeTypeErrorsForTestResult(file.name.letter1, -0.1563)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.diet.letter.1))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.14est.letter.1 <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.1_type14est_20180818110229"
  result.risk <- computeStopTimeForTestResult(file.name.14est.letter.1, true.value = -0.1563)
  file.name.letter1.14est <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.1_type14est_TRUE_20180818165826"
  result.err <- computeTypeErrorsForTestResult(file.name.letter1.14est, -0.1563)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.14est.letter.1))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.V.letter.1 <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.1_typeV_20180818112728"
  result.risk <- computeStopTimeForTestResult(file.name.V.letter.1, true.value = -0.1563)
  file.name.letter1.V <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.1_typeV_TRUE_20180818170604"
  result.err <- computeTypeErrorsForTestResult(file.name.letter1.V, -0.1563)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.V.letter.1))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  
  file.name.diet.letter2 <- "C:/Users/YJ/Desktop/a/result/diet_letter.2_20180818103626"
  result.risk <- computeStopTimeForTestResult(file.name.diet.letter2, true.value = -0.1030)
  file.name.letter2 <- "C:/Users/YJ/Desktop/a/result/diet_letter.2_TRUE_20180818165202"
  result.err <- computeTypeErrorsForTestResult(file.name.letter2, -0.1030)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.diet.letter2))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  
  file.name.14est.letter2 <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.2_type14est_20180818110153"
  result.risk <- computeStopTimeForTestResult(file.name.14est.letter2, true.value = -0.1030)
  file.name.letter2.14est <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.2_type14est_TRUE_20180818165921"
  result.err <- computeTypeErrorsForTestResult(file.name.letter2.14est, -0.1030)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.14est.letter2))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.V.letter2 <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.2_typeV_20180818113002"
  result.risk <- computeStopTimeForTestResult(file.name.V.letter2, true.value = -0.1030)
  file.name.letter2.V <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.2_typeV_TRUE_20180818170708"
  result.err <- computeTypeErrorsForTestResult(file.name.letter2.V, -0.1030)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.V.letter2))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.diet.letter3 <- "C:/Users/YJ/Desktop/a/result/diet_letter.3_20180818103615"
  result.risk <- computeStopTimeForTestResult(file.name.diet.letter3, true.value = -0.0652)
  file.name.letter3 <- "C:/Users/YJ/Desktop/a/result/diet_letter.3_TRUE_20180818165316"
  result.err <- computeTypeErrorsForTestResult(file.name.letter3, -0.0652)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.diet.letter3))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.14est.letter3 <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.3_type14est_20180818111146"
  result.risk <- computeStopTimeForTestResult(file.name.14est.letter3, true.value = -0.0652)
  file.name.letter3.14est <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.3_type14est_TRUE_20180818165724"
  result.err <- computeTypeErrorsForTestResult(file.name.letter3.14est, -0.0652)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.14est.letter3))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.V.letter3 <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.3_typeV_20180818112821"
  result.risk <- computeStopTimeForTestResult(file.name.V.letter3, true.value = -0.0652)
  file.name.letter3.V <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.3_typeV_TRUE_20180818170443"
  result.err <- computeTypeErrorsForTestResult(file.name.letter3.V, -0.0652)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.V.letter3))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.diet.letter4 <- "C:/Users/YJ/Desktop/a/result/diet_letter.4_20180818103124"
  result.risk <- computeStopTimeForTestResult(file.name.diet.letter4, true.value = -0.0302)
  file.name.letter4 <- "C:/Users/YJ/Desktop/a/result/diet_letter.4_TRUE_20180818164928"
  result.err <- computeTypeErrorsForTestResult(file.name.letter4, -0.0302)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.diet.letter4))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.14est.letter4 <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.4_type14est_20180818110733"
  result.risk <- computeStopTimeForTestResult(file.name.14est.letter4, true.value = -0.0302)
  file.name.letter4.14est <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.4_type14est_TRUE_20180818170435"
  result.err <- computeTypeErrorsForTestResult(file.name.letter4.14est, -0.0302)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.14est.letter4))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.V.letter4 <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.4_typeV_20180818112650"
  result.risk <- computeStopTimeForTestResult(file.name.V.letter4, true.value = -0.0302)
  file.name.letter4.V <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.4_typeV_TRUE_20180818170623"
  result.err <- computeTypeErrorsForTestResult(file.name.letter4.V, -0.0302)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.V.letter4))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  
  file.name.diet.letter5 <- "C:/Users/YJ/Desktop/a/result/diet_letter.5_20180818103225"
  result.risk <- computeStopTimeForTestResult(file.name.diet.letter5, true.value = -0.0071)
  file.name.letter5 <- "C:/Users/YJ/Desktop/a/result/diet_letter.5_TRUE_20180818165307"
  result.err <- computeTypeErrorsForTestResult(file.name.letter5, -0.0071)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.diet.letter5))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.14est.letter5 <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.5_type14est_20180818110623"
  result.risk <- computeStopTimeForTestResult(file.name.14est.letter5, true.value = -0.0071)
  file.name.letter5.14est <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.5_type14est_TRUE_20180818170046"
  result.err <- computeTypeErrorsForTestResult(file.name.letter5.14est, -0.0071)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.14est.letter5))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  
  file.name.V.letter5 <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.5_typeV_20180818112847"
  result.risk <- computeStopTimeForTestResult(file.name.V.letter5, true.value = -0.0071)
  file.name.letter5.V <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.5_typeV_TRUE_20180818170459"
  result.err <- computeTypeErrorsForTestResult(file.name.letter5.V, -0.0071)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.V.letter5))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  
  file.name.diet.letter6 <- "C:/Users/YJ/Desktop/a/result/diet_letter.6_20180818103557"
  result.risk <- computeStopTimeForTestResult(file.name.diet.letter6, true.value = 0.0807)
  file.name.letter6 <- "C:/Users/YJ/Desktop/a/result/diet_letter.6_TRUE_20180818165629"
  result.err <- computeTypeErrorsForTestResult(file.name.letter6, 0.0807)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.diet.letter6))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.14est.letter6 <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.6_type14est_20180818110527"
  result.risk <- computeStopTimeForTestResult(file.name.14est.letter6, true.value = 0.0807)
  file.name.letter6.14est <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.6_type14est_TRUE_20180818170227"
  result.err <- computeTypeErrorsForTestResult(file.name.letter6.14est, 0.0807)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.14est.letter6))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
  file.name.V.letter6 <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.6_typeV_20180818112852"
  result.risk <- computeStopTimeForTestResult(file.name.V.letter6, true.value = 0.0807)
  file.name.letter6.V <- "C:/Users/YJ/Desktop/a/result/bmx2_letter.6_typeV_TRUE_20180818170906"
  result.err <- computeTypeErrorsForTestResult(file.name.letter6.V, 0.0807)
  result.dectime <- formatMatrix(computeDecisionExpectedStopTimeForTestResult(file.name.V.letter6))
  print(result.risk)
  print(result.err)
  print(result.dectime)
  
}