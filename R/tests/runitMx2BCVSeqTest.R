# 基于mx2 bcv的序贯t-检验的测试代码。
#
# 测试了多种不同的配置下的序贯t-检验。
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/11/13

testMx2BCVSeqTestOnUCI_iris <- function() {
  source("./tasks/seq_significant_test.R", encoding="UTF-8")
  conf_TASK_BMX2SEQTEST <- list(
    alpha = 0.05,
    beta = 0.1,
    var.est.conf = list(
      name = "var_est3_mx2cv"
    ),
    delta_0 = 0.005,
    delta_1 = 0.03,
    upper_m = 16,
    lower_m = 3,
    dataset.conf = list(
      type =  "classification",
      name =  "uci_iris"
    ),
    algorithm1.conf = list(
      type = "classification",
      name = "maxClassifier"
    ),
    algorithm2.conf = list(
      type = "classification",
      name = "svm"
    ),
    crossvalidation.conf = list(
      name = "increaseBalancedMx2cv",
      m = 0
    )
  )
  task_config <- seq_significant_test.task_config_validation(conf_TASK_BMX2SEQTEST)
  result <- seq_significant_test.perform_task(task_config)
  print(result)
}

testMx2BCVSeqTestOnSCLA1 <- function(upper_m, sim_count) {
  source("./tasks/mx2cv_seq_significant_test.R", encoding="UTF-8")
  results <- c()
  conf_TASK_BMX2SEQTEST <- list(
    alpha = 0.05,
    beta = 0.1,
    var.est.conf = list(
      name = "var_est3_mx2cv"
    ),
    delta_0 = 0.02,
    delta_1 = 0.04,
    upper_m = upper_m,
    lower_m = 3,
    agree_cnt = 3,
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim1",
      sigma1 = diag(2)
    ),
    algorithm1.conf = list(
      name = "classificationTree",
      type = "classification",
      method = "class"
    ),
    algorithm2.conf = list (
      name = "linearRegrClassifier",
      type = "classification"
    ),
    crossvalidation.conf = list(
      name = "increaseBalancedMx2cv",
      m = 3
    )
  )
  for(i in 1:sim_count) {
    if(i%%10 == 0) cat(i,"...\t")
    task_config <- mx2cv_seq_significant_test.task_config_validation(conf_TASK_BMX2SEQTEST)
    result      <- mx2cv_seq_significant_test.perform_task(task_config)
    results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2]))
  }
  cat("\n")
  result.table <- c()
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==0),1], levels = conf_TASK_BMX2SEQTEST$lower_m:conf_TASK_BMX2SEQTEST$upper_m)))
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==1),1], levels = conf_TASK_BMX2SEQTEST$lower_m:conf_TASK_BMX2SEQTEST$upper_m)))
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==2),1], levels = conf_TASK_BMX2SEQTEST$lower_m:conf_TASK_BMX2SEQTEST$upper_m)))
  props <- t(t(rowSums(result.table)/sim_count))
  colnames(props) <- c("prop")
  result.table <- cbind(result.table, props)
  row.names(result.table) <- c("accept H0:", "accept H1:", "other:")
  return(result.table)
}


testMx2BCVSeqTestOnSCLA1_rmx2cv <- function(upper_m,sim_count) {
  source("./tasks/mx2cv_seq_significant_test.R", encoding="UTF-8")
  results <- c()
  conf_TASK_BMX2SEQTEST <- list(
    alpha = 0.05,
    beta = 0.1,
    var.est.conf = list(
      name = "var_est3_mx2cv"
    ),
    delta_0 = 0.02,
    delta_1 = 0.04,
    upper_m = upper_m,
    lower_m = 3,
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim1",
      sigma1 = diag(2)
    ),
    algorithm1.conf = list(
      name = "classificationTree",
      type = "classification",
      method = "class"
    ),
    algorithm2.conf = list (
      name = "linearRegrClassifier",
      type = "classification"
    ),
    crossvalidation.conf = list(
      name = "mxkrcv_inc",
      m = 3,
      v = 2
    )
  )
  for(i in 1:sim_count) {
    if(i%%10 == 0) cat(i,"...\t")
    task_config <- mx2cv_seq_significant_test.task_config_validation(conf_TASK_BMX2SEQTEST)
    result      <- mx2cv_seq_significant_test.perform_task(task_config)
    results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2]))
  }
  cat("\n")
  result.table <- c()
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==0),1], levels = conf_TASK_BMX2SEQTEST$lower_m:conf_TASK_BMX2SEQTEST$upper_m)))
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==1),1], levels = conf_TASK_BMX2SEQTEST$lower_m:conf_TASK_BMX2SEQTEST$upper_m)))
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==2),1], levels = conf_TASK_BMX2SEQTEST$lower_m:conf_TASK_BMX2SEQTEST$upper_m)))
  props <- t(t(rowSums(result.table)/sim_count))
  colnames(props) <- c("prop")
  result.table <- cbind(result.table, props)
  row.names(result.table) <- c("accept H0:", "accept H1:", "other:")
  return(result.table)
}


testMx2BCVSeqTestOnSCLA1_diet <- function(upper_m,sim_count) {
  source("./tasks/diet_mx2cv_seq_significant_test.R", encoding="UTF-8")
  results <- c()
  conf_TASK_BMX2SEQTEST <- list(
    alpha = 0.05,
    beta = 0.1,
    var.est.conf = list(
      name = "var_est_dietterich_mx2cv"
    ),
    delta_0 = 0.02,
    delta_1 = 0.04,
    upper_m = upper_m,
    lower_m = 3,
    agree_cnt = 3,
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim1",
      sigma1 = diag(2)
    ),
    algorithm1.conf = list(
      name = "classificationTree",
      type = "classification",
      method = "class"
    ),
    algorithm2.conf = list (
      name = "linearRegrClassifier",
      type = "classification"
    ),
    crossvalidation.conf = list(
      name = "increaseBalancedMx2cv",
      m = 3
    )
  )
  for(i in 1:sim_count) {
    if(i%%10 == 0) cat(i,"...\t")
    task_config <- diet_mx2cv_seq_significant_test.task_config_validation(conf_TASK_BMX2SEQTEST)
    result      <- diet_mx2cv_seq_significant_test.perform_task(task_config)
    results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2]))
  }
  cat("\n")
  result.table <- c()
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==0),1], levels = conf_TASK_BMX2SEQTEST$lower_m:conf_TASK_BMX2SEQTEST$upper_m)))
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==1),1], levels = conf_TASK_BMX2SEQTEST$lower_m:conf_TASK_BMX2SEQTEST$upper_m)))
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==2),1], levels = conf_TASK_BMX2SEQTEST$lower_m:conf_TASK_BMX2SEQTEST$upper_m)))
  props <- t(t(rowSums(result.table)/sim_count))
  colnames(props) <- c("prop")
  result.table <- cbind(result.table, props)
  row.names(result.table) <- c("accept H0:", "accept H1:", "other:")
  return(result.table)
}


testMx2BCVSeqTestOnSCLA1_diet_rmx2 <- function(upper_m,sim_count) {
  source("./tasks/diet_mx2cv_seq_significant_test.R", encoding="UTF-8")
  results <- c()
  conf_TASK_BMX2SEQTEST <- list(
    alpha = 0.05,
    beta = 0.1,
    var.est.conf = list(
      name = "var_est_dietterich_mx2cv"
    ),
    delta_0 = 0.02,
    delta_1 = 0.04,
    upper_m = upper_m,
    lower_m = 3,
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim1",
      sigma1 = diag(2)
    ),
    algorithm1.conf = list(
      name = "classificationTree",
      type = "classification",
      method = "class"
    ),
    algorithm2.conf = list (
      name = "linearRegrClassifier",
      type = "classification"
    ),
    crossvalidation.conf = list(
      name = "mxkrcv_inc",
      m = 3,
      v = 2
    )
  )
  for(i in 1:sim_count) {
    if(i%%10 == 0) cat(i,"...\t")
    task_config <- diet_mx2cv_seq_significant_test.task_config_validation(conf_TASK_BMX2SEQTEST)
    result      <- diet_mx2cv_seq_significant_test.perform_task(task_config)
    results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2]))
  }
  cat("\n")
  result.table <- c()
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==0),1], levels = conf_TASK_BMX2SEQTEST$lower_m:conf_TASK_BMX2SEQTEST$upper_m)))
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==1),1], levels = conf_TASK_BMX2SEQTEST$lower_m:conf_TASK_BMX2SEQTEST$upper_m)))
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==2),1], levels = conf_TASK_BMX2SEQTEST$lower_m:conf_TASK_BMX2SEQTEST$upper_m)))
  props <- t(t(rowSums(result.table)/sim_count))
  colnames(props) <- c("prop")
  result.table <- cbind(result.table, props)
  row.names(result.table) <- c("accept H0:", "accept H1:", "other:")
  return(result.table)
}

testMx2BCVSeqTestOnSCLA1_alpaydin <- function(upper_m,sim_count) {
  source("./tasks/alpaydin_mx2cv_seq_f_test.R", encoding="UTF-8")
  results <- c()
  conf_TASK_BMX2SEQTEST <- list(
    alpha = 0.05,
    beta = 0.1,
    var.est.conf = list(
      name = "var_est_dietterich_mx2cv"
    ),
    delta_0 = 0.02,
    delta_1 = 0.04,
    upper_m = upper_m,
    lower_m = 3,
    agree_cnt = 3,
    set_seed = F,
    print_verbose = F,
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim1",
      sigma1 = diag(2)
    ),
    algorithm1.conf = list(
      name = "classificationTree",
      type = "classification",
      method = "class"
    ),
    algorithm2.conf = list (
      name = "linearRegrClassifier",
      type = "classification"
    ),
    crossvalidation.conf = list(
      name = "increaseBalancedMx2cv",
      m = 3
    )
  )
  for(i in 1:sim_count) {
    if(i%%10 == 0) cat(i,"...\t")
    task_config <- alpaydin_mx2cv_seq_f_test.task_config_validation(conf_TASK_BMX2SEQTEST)
    result      <- alpaydin_mx2cv_seq_f_test.perform_task(task_config)
    results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2]))
  }
  cat("\n")
  result.table <- c()
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==0),1], levels = conf_TASK_BMX2SEQTEST$lower_m:conf_TASK_BMX2SEQTEST$upper_m)))
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==1),1], levels = conf_TASK_BMX2SEQTEST$lower_m:conf_TASK_BMX2SEQTEST$upper_m)))
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==2),1], levels = conf_TASK_BMX2SEQTEST$lower_m:conf_TASK_BMX2SEQTEST$upper_m)))
  props <- t(t(rowSums(result.table)/sim_count))
  colnames(props) <- c("prop")
  result.table <- cbind(result.table, props)
  row.names(result.table) <- c("accept H0:", "accept H1:", "other:")
  return(result.table)
}


testMx2BCVSimIntTaskOnSCLA1 <- function(upper_m, sim_count) {
  source("./tasks/mx2cv_seq_significant_test.R", encoding="UTF-8")
  results <- c()
  conf_TASK_BMX2SEQTEST <- list(
    alpha = 0.05,
    beta = 0.1,
    var.est.conf = list(
      name = "var_est3_mx2cv"
    ),
    delta_0 = 0.02,
    delta_1 = 0.04,
    upper_m = upper_m,
    lower_m = 3,
    sim_int = TRUE,
    agree_cnt = 16,
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim1",
      sigma1 = diag(2)
    ),
    algorithm1.conf = list(
      name = "classificationTree",
      type = "classification",
      method = "class"
    ),
    algorithm2.conf = list (
      name = "linearRegrClassifier",
      type = "classification"
    ),
    crossvalidation.conf = list(
      name = "increaseBalancedMx2cv",
      m = 3
    )
  )
  result.matrix <- c()
  for(i in 1:sim_count) {
    if(i%%10 == 0) cat(i,"...\t")
    task_config <- mx2cv_seq_significant_test.task_config_validation(conf_TASK_BMX2SEQTEST)
    result      <- mx2cv_seq_significant_test.perform_task(task_config)
    result.matrix <- cbind(result.matrix, result)
  }
  print("\n")
  true_conf_int <- cbind(t(t(rowMeans(result.matrix[,seq(1, ncol(result.matrix),2)]))), t(t(rowMeans(result.matrix[,seq(2, ncol(result.matrix),2)]))))
  row.names(true_conf_int) <- conf_TASK_BMX2SEQTEST$lower_m:conf_TASK_BMX2SEQTEST$upper_m
  colnames(true_conf_int) <- c("I_l", "I_r")
  print(true_conf_int)
  return(true_conf_int)
}


