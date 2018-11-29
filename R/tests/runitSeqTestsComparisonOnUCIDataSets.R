# 在UCI数据集上进行序贯检验。
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2018/2/3

testsAllUCIDataSets_98diet <- function(upper_m, delta0, delta1, alpha, beta, cores = 20, agr.cnt=3) {
  assign("last.warning", NULL, envir = baseenv())
  uci.datanames <- c("artificial","hayesroth","pima",
                     "heart","balance","hepatitis","promoters","bupa",
                     "satellite47","car","iris","spambase","cmc","ironosphere","spect","credit",
                     "krvskp","tae","cylinder","letter","tic_tac_toe","dermatology","magic","titanic",
                     "mammographic","transfusion","ecoli","monk","vehicle","flags","nursery",
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
  # 序贯检验开始
  cores <- cores
  library(foreach)
  library(doParallel)
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  results <- foreach(idx = 1: n.all.exprs, .combine = rbind, .export = ls(.GlobalEnv)) %dopar% {
    source("./tasks/diet_mx2cv_seq_significant_test.R", encoding="UTF-8")
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

    task_config <- list(
      alpha = alpha,
      beta  = beta,
      agree_cnt = agr.cnt,
      var.est.conf = list(
        name = "var_est_dietterich_mx2cv"
      ),
      delta_0 = delta0,
      delta_1 = delta1,
      upper_m = upper_m,
      lower_m = 3,
      dataset.conf = dataset.conf,
      algorithm1.conf = algor1.conf,
      algorithm2.conf = algor2.conf,
      crossvalidation.conf = list(
        name = "mx2bcv_inc",
        m = 3
      )
    )
    task_config <- diet_mx2cv_seq_significant_test.task_config_validation(task_config)
    result      <- diet_mx2cv_seq_significant_test.perform_task(task_config)
    result$test.result
  }
  stopCluster(cl)
  write.csv(results, file = paste("test_result/diet", "uci", "datasets", "upper_m", upper_m, "alpha",alpha, "beta", 
                                  beta,"agrcnt", agr.cnt, ".csv", sep="_"))
}

testsAllUCIDataSets_13comb <-  function(upper_m, delta0, delta1, alpha, beta, cores = 20, agr.cnt=3)  {
  assign("last.warning", NULL, envir = baseenv())
  uci.datanames <- c("artificial","hayesroth","pima",
                     "heart","balance","hepatitis","promoters","bupa",
                     "satellite47","car","iris","spambase","cmc","ironosphere","spect","credit",
                     "krvskp","tae","cylinder","letter","tic_tac_toe","dermatology","magic","titanic",
                     "mammographic","transfusion","ecoli","monk","vehicle","flags","nursery",
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
  # 序贯检验开始
  cores <- cores
  library(foreach)
  library(doParallel)
  cl <- makeCluster(cores)
  registerDoParallel(cl)
  results <- foreach(idx = 1: n.all.exprs, .combine = rbind, .export = ls(.GlobalEnv)) %dopar% {
    source("./tasks/combined_mx2cv_seq_t_test.R", encoding="UTF-8")
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
    task_config <- list(
      alpha = alpha,
      beta  = beta,
      agree_cnt = agr.cnt,
      var.est.conf = list(
        name = "var_est_dietterich_mx2cv"
      ),
      delta_0 = delta0,
      delta_1 = delta1,
      upper_m = upper_m,
      lower_m = 3,
      dataset.conf = dataset.conf,
      algorithm1.conf = algor1.conf,
      algorithm2.conf = algor2.conf,
      crossvalidation.conf = list(
        name = "mx2bcv_inc",
        m = 3
      )
    )
    task_config <- combined_mx2cv_seq_t_test.task_config_validation(task_config)
    result      <- combined_mx2cv_seq_t_test.perform_task(task_config)
    result$test.result
  }
  stopCluster(cl)
  write.csv(results, file = paste("test_result/comb", "uci", "datasets", "upper_m", upper_m, "alpha",alpha, "beta", 
                                  beta,"agrcnt", agr.cnt, ".csv", sep="_"))
}


testsAllUCIDataSets_bmx2_prototype <-  function(upper_m, delta0, delta1, alpha, beta, est.type ="typeI", cores = 20, agr.cnt=3) {
  assign("last.warning", NULL, envir = baseenv())
  uci.datanames <- c("artificial","hayesroth","pima",
                     "heart","balance","hepatitis","promoters","bupa",
                     "satellite47","car","iris","spambase","cmc","ironosphere","spect","credit",
                     "krvskp","tae","cylinder","letter","tic_tac_toe","dermatology","magic","titanic",
                     "mammographic","transfusion","ecoli","monk","vehicle","flags","nursery",
                     "vote","flare","optdigits","wave","german","page_block","wine","glass","parkinsons",
                     "yeast","haberman","pendigits","zoo")
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
  # 序贯检验开始
  cores <- cores
  library(foreach)
  library(doParallel)
  cl <- makeCluster(cores) #type="MPI"
  registerDoParallel(cl)
  results <- foreach(idx = 1: n.all.exprs, .combine = rbind, .export = ls(.GlobalEnv)) %dopar% {
    source("./tasks/mx2cv_seq_significant_test.R", encoding="UTF-8")
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
    task_config <- list(
      est_type = est.type,
      alpha = alpha,
      beta  = beta,
      agree_cnt = agr.cnt,
      var.est.conf = list(
        name = "var_est3_mx2cv"
      ),
      delta_0 = delta0,
      delta_1 = delta1,
      upper_m = upper_m,
      lower_m = 3,
      dataset.conf = dataset.conf,
      algorithm1.conf = algor1.conf,
      algorithm2.conf = algor2.conf,
      crossvalidation.conf = list(
        name = "mx2bcv_inc",
        m = 3
      )
    )
    task_config <- mx2cv_seq_significant_test.task_config_validation(task_config)
    result      <- mx2cv_seq_significant_test.perform_task(task_config)
    result$test.result
  }
  stopCluster(cl)
  return(results)
} 


testsAllUCIDataSets_bmx2 <- function(upper_m, delta0, delta1, alpha, beta, cores = 20, agr.cnt=3) {
  testsAllUCIDataSets_bmx2(upper_m, delta0, delta1, alpha, beta, "typeI", cores, agr.cnt)
  write.csv(results, file = paste("test_result/bmx2", "uci", "datasets", "upper_m", upper_m, "alpha",alpha, "beta", 
                                beta,"agrcnt", agr.cnt, ".csv", sep="_"))
}

testsAllUCIDataSets_14bmx2 <- function(upper_m, delta0, delta1, alpha, beta, cores = 20, agr.cnt=3) {
  testsAllUCIDataSets_bmx2(upper_m, delta0, delta1, alpha, beta, "14est", cores, agr.cnt)
  write.csv(results, file = paste("test_result/14bmx2", "uci", "datasets", "upper_m", upper_m, "alpha",alpha, "beta", 
                                  beta,"agrcnt", agr.cnt, ".csv", sep="_"))
}

testsAllUCIDataSets_typeII <- function(upper_m, delta0, delta1, alpha, beta, cores = 20, agr.cnt=3) {
  testsAllUCIDataSets_bmx2(upper_m, delta0, delta1, alpha, beta, "typeII", cores, agr.cnt)
  write.csv(results, file = paste("test_result/typeII_bmx2", "uci", "datasets", "upper_m", upper_m, "alpha",alpha, "beta", 
                                  beta,"agrcnt", agr.cnt, ".csv", sep="_"))
}
