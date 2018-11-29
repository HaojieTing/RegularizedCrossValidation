# 对比多种序贯t-检验的测试代码。
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/12/30

testMx2BCVSeqTestOnSCLA3_bmx2cv <- function(upper_m, sim_count, muvec, delta0, delta1, est.type="typeI", agr.cnt=3, seq=T, m.fixed=NULL) {
  source("./tasks/mx2cv_seq_significant_test.R", encoding="UTF-8")
  results <- c()
  conf_TASK_BMX2SEQTEST <- list(
    est_type = est.type,
    alpha = 0.05,
    beta  = 0.05,
    agree_cnt = agr.cnt,
    var.est.conf = list(
      name = "var_est3_mx2cv"
    ),
    delta_0 = delta0,
    delta_1 = delta1,
    upper_m = upper_m,
    fixed_m = m.fixed,
    lower_m = 3,
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim3",
      mu1 = muvec
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
  if(seq == T) {
    for(i in 1:sim_count) {
      set.seed(123+i)
      if(i%%10 == 0) cat(i,"...\t")
      task_config <- mx2cv_seq_significant_test.task_config_validation(conf_TASK_BMX2SEQTEST)
      result      <- mx2cv_seq_significant_test.perform_task(task_config)
      results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2],result$test.result[1,5],result$test.result[1,6]))
      #print(result$verbose)
    }
  } else {
    library(foreach)
    library(doParallel)
    cl <- makeCluster(4)
    registerDoParallel(cl)
    work.directory <- getwd()
    results <- foreach(i = 1:sim_count, .combine = rbind, .export = ls(.GlobalEnv)) %dopar% {
      setwd(work.directory)
      set.seed(123+i)
      task_config <- mx2cv_seq_significant_test.task_config_validation(conf_TASK_BMX2SEQTEST)
      result      <- mx2cv_seq_significant_test.perform_task(task_config)
      c(result$test.result[1,1],result$test.result[1,2],result$test.result[1,5],result$test.result[1,6], result$mu.vec)
    }
    stopCluster(cl)
  }
  m.stop  <- upper_m + agr.cnt
  result.table <- c()
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==0),1], levels = conf_TASK_BMX2SEQTEST$lower_m:m.stop)))
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==1),1], levels = conf_TASK_BMX2SEQTEST$lower_m:m.stop)))
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==2),1], levels = conf_TASK_BMX2SEQTEST$lower_m:m.stop)))
  type1error <- mean(results[,3])
  type2error <- mean(results[,4])
  props <- t(t(rowSums(result.table)/sim_count))
  colnames(props) <- c("prop")
  result.table <- cbind(result.table, props)
  row.names(result.table) <- c("accept H0:", "accept H1:", "other:")
  if(seq == F){
    suffix <- muvec[1]
    repi <- sim_count
    upperm <- upper_m
    write.csv(props, paste(paste("test_result/sp_scla_err_bmx2_", est.type, "_", sep=""),  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
    write.csv(result.table, paste(paste("test_result/sp_scla_mdist_bmx2_", est.type, "_", sep=""),  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
    write.csv(type1error, paste(paste("test_result/sp_scla_type1error_bmx2_", est.type, "_", sep=""), "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
    write.csv(type2error, paste(paste("test_result/sp_scla_type2error_bmx2_", est.type, "_", sep=""), "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
    save(results, file=paste("results_scla", sim_count, m.fixed, "rdata", sep = "."))
  }
  return(list(result.table, type1error, type2error))
}


testMx2BCVSeqTestOnSCLA3_dietterich <- function(upper_m, sim_count, muvec, delta0, delta1, agr.cnt=3, seq = T) {
  source("./tasks/diet_mx2cv_seq_significant_test.R", encoding="UTF-8")
  results <- c()
  conf_task_dietterich <- list(
    alpha = 0.05,
    beta  = 0.05,
    agree_cnt = agr.cnt,
    var.est.conf = list(
      name = "var_est_dietterich_mx2cv"
    ),
    delta_0 = delta0,
    delta_1 = delta1,
    upper_m = upper_m,
    lower_m = 3,
    dataset.conf = list(
      name = "two_normals_classification",
      type = "classification",
      shortcut_name = "bengio_infer_ml_sim3",
      mu1 = muvec
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
  if(seq == T) {
    for(i in 1:sim_count) {
      set.seed(123+i)
      if(i%%10 == 0) cat(i,"...\t")
      task_config <- diet_mx2cv_seq_significant_test.task_config_validation(conf_task_dietterich)
      result      <- diet_mx2cv_seq_significant_test.perform_task(task_config)
      results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2],result$test.result[1,5],result$test.result[1,6]))
      #print(result$verbose)
    }
  } else {
    library(foreach)
    library(doParallel)
    cl <- makeCluster(20)
    registerDoParallel(cl)
    work.directory <- getwd()
    results <- foreach(i = 1:sim_count, .combine = rbind, .export = ls(.GlobalEnv)) %dopar% {
      setwd(work.directory)
      set.seed(123+i)
      task_config <- diet_mx2cv_seq_significant_test.task_config_validation(conf_task_dietterich)
      result      <- diet_mx2cv_seq_significant_test.perform_task(task_config)
      c(result$test.result[1,1],result$test.result[1,2],result$test.result[1,5],result$test.result[1,6])
    }
    stopCluster(cl)
  }
  m.stop  <- upper_m + agr.cnt
  result.table <- c()
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==0),1], levels = conf_task_dietterich$lower_m:m.stop)))
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==1),1], levels = conf_task_dietterich$lower_m:m.stop)))
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==2),1], levels = conf_task_dietterich$lower_m:m.stop)))
  type1error <- mean(results[,3])
  type2error <- mean(results[,4])
  props <- t(t(rowSums(result.table)/sim_count))
  colnames(props) <- c("prop")
  result.table <- cbind(result.table, props)
  row.names(result.table) <- c("accept H0:", "accept H1:", "other:")
  if(seq == F){
    suffix <- muvec[1]
    repi <- sim_count
    upperm <- upper_m
    write.csv(props, paste("test_result/sp_scla_err_diet_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
    write.csv(result.table, paste("test_result/sp_scla_mdist_diet_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix,"csv", sep="."))
    write.csv(type1error, paste("test_result/sp_scla_type1error_diet_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix,"csv", sep="."))
    write.csv(type2error, paste("test_result/sp_scla_type2error_diet_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix,"csv", sep="."))
  }
  return(list(result.table, type1error, type2error))
}


# testMx2BCVSeqTestOnSCLA3_combinedmx2cv <- function(upper_m, sim_count, muvec, delta0, delta1, alpha=0.05, beta=0.05, agr.cnt=3) {
#   source("./tasks/combined_mx2cv_seq_t_test.R", encoding="UTF-8")
#   results <- c()
#   conf_task_combined <- list(
#     alpha = alpha,
#     beta  = beta,
#     agree_cnt = agr.cnt,
#     var.est.conf = list(
#       name = "var_est_dietterich_mx2cv"
#     ),
#     delta_0 = delta0,
#     delta_1 = delta1,
#     upper_m = upper_m,
#     lower_m = 3,
#     dataset.conf = list(
#       name = "two_normals_classification",
#       type = "classification",
#       shortcut_name = "bengio_infer_ml_sim3",
#       mu1 = muvec
#     ),
#     algorithm2.conf = list(
#       name = "classificationTree",
#       type = "classification",
#       method = "class"
#     ),
#     algorithm1.conf = list (
#       name = "linearRegrClassifier",
#       type = "classification"
#     ),
#     crossvalidation.conf = list(
#       name = "mx2bcv_inc",
#       m = 3
#     )
#   )
#   for(i in 1:sim_count) {
#     set.seed(123+i)
#     if(i%%10 == 0) cat(i,"...\t")
#     task_config <- combined_mx2cv_seq_t_test.task_config_validation(conf_task_combined)
#     result      <- combined_mx2cv_seq_t_test.perform_task(task_config)
#     results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2]))
#   }
#   cat("\n")
#   m.stop <- upper_m + agr.cnt
#   result.table <- c()
#   result.table <- rbind(result.table, table(factor(results[which(results[,2]==0),1], levels = conf_task_combined$lower_m:m.stop)))
#   result.table <- rbind(result.table, table(factor(results[which(results[,2]==1),1], levels = conf_task_combined$lower_m:m.stop)))
#   result.table <- rbind(result.table, table(factor(results[which(results[,2]==2),1], levels = conf_task_combined$lower_m:m.stop)))
#   props <- t(t(rowSums(result.table)/sim_count))
#   colnames(props) <- c("prop")
#   result.table <- cbind(result.table, props)
#   row.names(result.table) <- c("accept H0:", "accept H1:", "other:")
#   return(result.table)
# }

testMultipleTimesTest_SCLA_diet <- function(mu1v, upperm, repi, delta0, delta1, cores, agr.cnt) {
  results <- c()
  occu.vec <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  mu1v.length <- length(mu1v)
  work.directory <- getwd()
  results <- foreach(idx = 1: mu1v.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    mu <- mu1v[idx]
    result <- testMx2BCVSeqTestOnSCLA3_dietterich(upperm, repi, rep(mu,2), delta0, delta1, agr.cnt = agr.cnt)
    result.first <- result[[1]]
    prop.col <- result.first[,ncol(result.first)]
    count.cols <- result.first[,1:(ncol(result.first)-1)]
    count.vec <- c(t(count.cols))
    type1error <- result[[2]]
    type2error <- result[[3]]
    list(prop.col, count.vec, type1error, type2error)
  }
  stopCluster(cl)
  m.stop <- upperm + agr.cnt
  props <- do.call(rbind,lapply(results, "[[", 1))
  occus <- do.call(rbind,lapply(results, "[[", 2))
  props <- t(props)
  type1errors <- do.call(cbind,lapply(results, "[[", 3))
  type2errors <- do.call(cbind,lapply(results, "[[", 4))
  colnames(props) <- mu1v
  rownames(occus) <- mu1v
  m.stop  <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus,type1errors,type2errors))
}

# testMultipleTimesTest_SCLA_comb <- function(mu1v, upperm, repi, delta0, delta1, alpha=0.05, beta=0.05, cores=20, agr.cnt=3) {
#   results <- c()
#   occu.vec <- c()
#   library(foreach)
#   library(doParallel)
#   cl <- makeCluster(cores)
#   registerDoParallel(cl)
#   mu1v.length <- length(mu1v)
#   results <- foreach(idx = 1: mu1v.length, .export = ls(.GlobalEnv)) %dopar% {
#     mu <- mu1v[idx]
#     result <- testMx2BCVSeqTestOnSCLA3_combinedmx2cv(upperm, repi, rep(mu,2), delta0, delta1, alpha, beta,agr.cnt = agr.cnt)
#     prop.col <- result[,ncol(result)]
#     count.cols <- result[,1:(ncol(result)-1)]
#     count.vec <- c(t(count.cols))
#     list(prop.col, count.vec)
#   }
#   stopCluster(cl)
#   props <- do.call(rbind,lapply(results, "[[", 1))
#   occus <- do.call(rbind,lapply(results, "[[", 2))
#   props <- t(props)
#   colnames(props) <- mu1v
#   rownames(occus) <- mu1v
#   m.stop <- upperm + agr.cnt
#   print(m.stop)
#   colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
#   return(list(props, occus))
# }

testMultipleTimesTest_SCLA_bmx2 <- function(mu1v, upperm, repi, delta0, delta1, cores, agr.cnt) {
  results <- c()
  occu.vec <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  mu1v.length <- length(mu1v)
  work.directory <- getwd()
  results <- foreach(idx = 1: mu1v.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    mu <- mu1v[idx]
    result <- testMx2BCVSeqTestOnSCLA3_bmx2cv(upperm, repi, rep(mu,2), delta0, delta1, est.type = "typeI", agr.cnt)
    result.first <- result[[1]]
    prop.col <- result.first[,ncol(result.first)]
    count.cols <- result.first[,1:(ncol(result.first)-1)]
    count.vec <- c(t(count.cols))
    type1error <- result[[2]]
    type2error <- result[[3]]
    list(prop.col, count.vec, type1error, type2error)
  }
  stopCluster(cl)
  props <- do.call(rbind,lapply(results, "[[", 1))
  occus <- do.call(rbind,lapply(results, "[[", 2))
  props <- t(props)
  type1errors <- do.call(cbind,lapply(results, "[[", 3))
  type2errors <- do.call(cbind,lapply(results, "[[", 4))
  colnames(props) <- mu1v
  rownames(occus) <- mu1v
  m.stop <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus, type1errors, type2errors))
}

testMultipleTimesTest_SCLA_bmx214 <- function(mu1v, upperm, repi, delta0, delta1, cores, agr.cnt) {
  results <- c()
  occu.vec <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  mu1v.length <- length(mu1v)
  work.directory <- getwd()
  results <- foreach(idx = 1: mu1v.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    mu <- mu1v[idx]
    result <- testMx2BCVSeqTestOnSCLA3_bmx2cv(upperm, repi, rep(mu,2), delta0, delta1, est.type = "14est", agr.cnt = agr.cnt)
    result.first <- result[[1]]
    prop.col <- result.first[,ncol(result.first)]
    count.cols <- result.first[,1:(ncol(result.first)-1)]
    count.vec <- c(t(count.cols))
    type1error <- result[[2]]
    type2error <- result[[3]]
    list(prop.col, count.vec, type1error, type2error)
  }
  stopCluster(cl)
  props <- do.call(rbind,lapply(results, "[[", 1))
  occus <- do.call(rbind,lapply(results, "[[", 2))
  props <- t(props)
  type1errors <- do.call(cbind,lapply(results, "[[", 3))
  type2errors <- do.call(cbind,lapply(results, "[[", 4))
  colnames(props) <- mu1v
  rownames(occus) <- mu1v
  m.stop <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus, type1errors, type2errors))
}


testMultipleTimesTest_SCLA_bmx2_typeV <- function(mu1v, upperm, repi, delta0, delta1, cores, agr.cnt) {
  results <- c()
  occu.vec <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  mu1v.length <- length(mu1v)
  work.directory <- getwd()
  results <- foreach(idx = 1: mu1v.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    mu <- mu1v[idx]
    result <- testMx2BCVSeqTestOnSCLA3_bmx2cv(upperm, repi, rep(mu,2), delta0, delta1, est.type = "typeV", agr.cnt)
    result.first <- result[[1]]
    prop.col <- result.first[,ncol(result.first)]
    count.cols <- result.first[,1:(ncol(result.first)-1)]
    count.vec <- c(t(count.cols))
    type1error <- result[[2]]
    type2error <- result[[3]]
    list(prop.col, count.vec, type1error, type2error)
  }
  stopCluster(cl)
  props <- do.call(rbind,lapply(results, "[[", 1))
  occus <- do.call(rbind,lapply(results, "[[", 2))
  props <- t(props)
  type1errors <- do.call(cbind,lapply(results, "[[", 3))
  type2errors <- do.call(cbind,lapply(results, "[[", 4))
  colnames(props) <- mu1v
  rownames(occus) <- mu1v
  m.stop <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus, type1errors, type2errors))
}


testMultipleTimesTest_SCLA_bmx2_typeVI <- function(mu1v, upperm, repi, delta0, delta1, cores, agr.cnt) {
  results <- c()
  occu.vec <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  mu1v.length <- length(mu1v)
  work.directory <- getwd()
  results <- foreach(idx = 1: mu1v.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    mu <- mu1v[idx]
    result <- testMx2BCVSeqTestOnSCLA3_bmx2cv(upperm, repi, rep(mu,2), delta0, delta1, est.type = "typeVI", agr.cnt)
    result.first <- result[[1]]
    prop.col <- result.first[,ncol(result.first)]
    count.cols <- result.first[,1:(ncol(result.first)-1)]
    count.vec <- c(t(count.cols))
    type1error <- result[[2]]
    type2error <- result[[3]]
    list(prop.col, count.vec, type1error, type2error)
  }
  stopCluster(cl)
  props <- do.call(rbind,lapply(results, "[[", 1))
  occus <- do.call(rbind,lapply(results, "[[", 2))
  props <- t(props)
  type1errors <- do.call(cbind,lapply(results, "[[", 3))
  type2errors <- do.call(cbind,lapply(results, "[[", 4))
  colnames(props) <- mu1v
  rownames(occus) <- mu1v
  m.stop <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus, type1errors, type2errors))
}


testMultipleTimesTest_SCLA_bmx2_typeVII <- function(mu1v, upperm, repi, delta0, delta1, cores, agr.cnt) {
  results <- c()
  occu.vec <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  mu1v.length <- length(mu1v)
  work.directory <- getwd()
  results <- foreach(idx = 1: mu1v.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    mu <- mu1v[idx]
    result <- testMx2BCVSeqTestOnSCLA3_bmx2cv(upperm, repi, rep(mu,2), delta0, delta1, est.type = "typeVII", agr.cnt)
    result.first <- result[[1]]
    prop.col <- result.first[,ncol(result.first)]
    count.cols <- result.first[,1:(ncol(result.first)-1)]
    count.vec <- c(t(count.cols))
    type1error <- result[[2]]
    type2error <- result[[3]]
    list(prop.col, count.vec, type1error, type2error)
  }
  stopCluster(cl)
  props <- do.call(rbind,lapply(results, "[[", 1))
  occus <- do.call(rbind,lapply(results, "[[", 2))
  props <- t(props)
  type1errors <- do.call(cbind,lapply(results, "[[", 3))
  type2errors <- do.call(cbind,lapply(results, "[[", 4))
  colnames(props) <- mu1v
  rownames(occus) <- mu1v
  m.stop <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus, type1errors, type2errors))
}


testMultipleTimesTest_SCLA_bmx2_typeVIII <- function(mu1v, upperm, repi, delta0, delta1, cores, agr.cnt) {
  results <- c()
  occu.vec <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  mu1v.length <- length(mu1v)
  work.directory <- getwd()
  results <- foreach(idx = 1: mu1v.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    mu <- mu1v[idx]
    result <- testMx2BCVSeqTestOnSCLA3_bmx2cv(upperm, repi, rep(mu,2), delta0, delta1, est.type = "typeVIII", agr.cnt)
    result.first <- result[[1]]
    prop.col <- result.first[,ncol(result.first)]
    count.cols <- result.first[,1:(ncol(result.first)-1)]
    count.vec <- c(t(count.cols))
    type1error <- result[[2]]
    type2error <- result[[3]]
    list(prop.col, count.vec, type1error, type2error)
  }
  stopCluster(cl)
  props <- do.call(rbind,lapply(results, "[[", 1))
  occus <- do.call(rbind,lapply(results, "[[", 2))
  props <- t(props)
  type1errors <- do.call(cbind,lapply(results, "[[", 3))
  type2errors <- do.call(cbind,lapply(results, "[[", 4))
  colnames(props) <- mu1v
  rownames(occus) <- mu1v
  m.stop <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus, type1errors, type2errors))
}

testMultipleTimesTest_SCLA_bmx2_typeIX <- function(mu1v, upperm, repi, delta0, delta1, cores, agr.cnt) {
  results <- c()
  occu.vec <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  mu1v.length <- length(mu1v)
  work.directory <- getwd()
  results <- foreach(idx = 1: mu1v.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    mu <- mu1v[idx]
    result <- testMx2BCVSeqTestOnSCLA3_bmx2cv(upperm, repi, rep(mu,2), delta0, delta1, est.type = "typeIX", agr.cnt)
    result.first <- result[[1]]
    prop.col <- result.first[,ncol(result.first)]
    count.cols <- result.first[,1:(ncol(result.first)-1)]
    count.vec <- c(t(count.cols))
    type1error <- result[[2]]
    type2error <- result[[3]]
    list(prop.col, count.vec, type1error, type2error)
  }
  stopCluster(cl)
  props <- do.call(rbind,lapply(results, "[[", 1))
  occus <- do.call(rbind,lapply(results, "[[", 2))
  props <- t(props)
  type1errors <- do.call(cbind,lapply(results, "[[", 3))
  type2errors <- do.call(cbind,lapply(results, "[[", 4))
  colnames(props) <- mu1v
  rownames(occus) <- mu1v
  m.stop <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus, type1errors, type2errors))
}


testMultipleTimesTest_SCLA_bmx2_typeX <- function(mu1v, upperm, repi, delta0, delta1, cores, agr.cnt) {
  results <- c()
  occu.vec <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  mu1v.length <- length(mu1v)
  work.directory <- getwd()
  results <- foreach(idx = 1: mu1v.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    mu <- mu1v[idx]
    result <- testMx2BCVSeqTestOnSCLA3_bmx2cv(upperm, repi, rep(mu,2), delta0, delta1, est.type = "typeX", agr.cnt)
    result.first <- result[[1]]
    prop.col <- result.first[,ncol(result.first)]
    count.cols <- result.first[,1:(ncol(result.first)-1)]
    count.vec <- c(t(count.cols))
    type1error <- result[[2]]
    type2error <- result[[3]]
    list(prop.col, count.vec, type1error, type2error)
  }
  stopCluster(cl)
  props <- do.call(rbind,lapply(results, "[[", 1))
  occus <- do.call(rbind,lapply(results, "[[", 2))
  props <- t(props)
  type1errors <- do.call(cbind,lapply(results, "[[", 3))
  type2errors <- do.call(cbind,lapply(results, "[[", 4))
  colnames(props) <- mu1v
  rownames(occus) <- mu1v
  m.stop <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus, type1errors, type2errors))
}

testMultipleTimesTest_SCLA_bmx2_type_general <- function(mu1v, upperm, repi, delta0, delta1, agr.cnt, version) {
  results <- c()
  occu.vec <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  mu1v.length <- length(mu1v)
  work.directory <- getwd()
  results <- foreach(idx = 1: mu1v.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    mu <- mu1v[idx]
    result <- testMx2BCVSeqTestOnSCLA3_bmx2cv(upperm, repi, rep(mu,2), delta0, delta1, est.type = paste("type", version, sep = ""), agr.cnt)
    result.first <- result[[1]]
    prop.col <- result.first[,ncol(result.first)]
    count.cols <- result.first[,1:(ncol(result.first)-1)]
    count.vec <- c(t(count.cols))
    type1error <- result[[2]]
    type2error <- result[[3]]
    list(prop.col, count.vec, type1error, type2error)
  }
  stopCluster(cl)
  props <- do.call(rbind,lapply(results, "[[", 1))
  occus <- do.call(rbind,lapply(results, "[[", 2))
  props <- t(props)
  type1errors <- do.call(cbind,lapply(results, "[[", 3))
  type2errors <- do.call(cbind,lapply(results, "[[", 4))
  colnames(props) <- mu1v
  rownames(occus) <- mu1v
  m.stop <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus, type1errors, type2errors))
}

# testMultipleTimesTest_SCLA_bmx2_typeII <- function(mu1v, upperm, repi, delta0, delta1, cores, agr.cnt) {
#   results <- c()
#   occu.vec <- c()
#   library(foreach)
#   library(doParallel)
#   cl <- makeCluster(cores)
#   registerDoParallel(cl)
#   mu1v.length <- length(mu1v)
#   results <- foreach(idx = 1: mu1v.length, .export = ls(.GlobalEnv)) %dopar% {
#     mu <- mu1v[idx]
#     result <- testMx2BCVSeqTestOnSCLA3_bmx2cv(upperm, repi, rep(mu,2), delta0, delta1, est.type = "typeII", agr.cnt = agr.cnt)
#     prop.col <- result[,ncol(result)]
#     count.cols <- result[,1:(ncol(result)-1)]
#     count.vec <- c(t(count.cols))
#     list(prop.col, count.vec)
#   }
#   stopCluster(cl)
#   m.stop <- upperm+agr.cnt
#   props <- do.call(rbind,lapply(results, "[[", 1))
#   occus <- do.call(rbind,lapply(results, "[[", 2))
#   props <- t(props)
#   colnames(props) <- mu1v
#   rownames(occus) <- mu1v
#   colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
#   return(list(props, occus))
# }

testDietOnSCLA  <- function(agr.cnt, d0, d1, upperm=11, repi=1000) {
  configs <- seq(0.2, 1, 0.01)
  configs <- c(0.15, configs)
  delta0 <- d0
  delta1 <- d1
  cores <- 20
  results <- testMultipleTimesTest_SCLA_diet(configs, upperm, repi, delta0, delta1,cores, agr.cnt = agr.cnt) 
  print(results)
  write.csv(results[[1]], paste("test_result/scla_err_diet_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/scla_mdist_diet_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste("test_result/scla_type1error_diet_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste("test_result/scla_type2error_diet_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}

# testCombOnSCLA  <- function(agr.cnt) {
#   configs <- seq(0.2, 1, 0.01)
#   configs <- c(0.15, configs)
#   upperm <- 11
#   repi <- 1000
#   delta0 <- 0.01
#   delta1 <- 0.03
#   alpha <- 0.05
#   beta <- 0.05
#   cores <- 20
#   results <- testMultipleTimesTest_SCLA_comb(configs, upperm, repi, delta0, delta1, alpha, beta, agr.cnt = agr.cnt) 
#   write.csv(results[[1]], paste("scla_err_comb_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, alpha, beta, "csv", sep="."))
#   write.csv(results[[2]], paste("scla_mdist_comb_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1,alpha, beta,  "csv", sep="."))
# }

# testCombOnSCLA_largem  <- function(agr.cnt) {
#   configs <- seq(0.2, 1, 0.01)
#   configs <- c(0.15, configs)
#   upperm <- 50
#   repi <- 1000
#   delta0 <- 0.01
#   delta1 <- 0.03
#   alpha <- 0.05
#   beta <- 0.05
#   cores <- 20
#   results <- testMultipleTimesTest_SCLA_comb(configs, upperm, repi, delta0, delta1, alpha, beta, agr.cnt = agr.cnt) 
#   write.csv(results[[1]], paste("test_result/scla_err_comb_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, alpha, beta, "csv", sep="."))
#   write.csv(results[[2]], paste("test_result/scla_mdist_comb_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, alpha, beta,  "csv", sep="."))
# }

testmx2OnSCLA  <- function(agr.cnt, d0, d1, upperm=11, repi=1000) {
  configs <- seq(0.2, 1, 0.01)
  configs <- c(0.15, configs)
  delta0 <- d0
  delta1 <- d1
  cores <- 20
  results <- testMultipleTimesTest_SCLA_bmx2(configs, upperm, repi, delta0, delta1,cores, agr.cnt = agr.cnt) 
  write.csv(results[[1]], paste("test_result/scla_err_bmx2_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/scla_mdist_bmx2_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste("test_result/scla_type1error_bmx2_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste("test_result/scla_type2error_bmx2_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}

testmx2OnSCLA14  <- function(agr.cnt, d0, d1, upperm=11, repi=1000) {
  configs <- seq(0.2, 1, 0.01)
  configs <- c(0.15, configs)
  delta0 <- d0
  delta1 <- d1
  cores <- 20
  results <- testMultipleTimesTest_SCLA_bmx214(configs, upperm, repi, delta0, delta1, cores, agr.cnt = agr.cnt) 
  write.csv(results[[1]], paste("test_result/scla_err_bmx214_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/scla_mdist_bmx214_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste("test_result/scla_type1error_bmx214_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste("test_result/scla_type2error_bmx214_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}

# testmx2OnSCLA_typeII  <- function(agr.cnt) {
#   configs <- seq(0.2, 1, 0.01)
#   configs <- c(0.15, configs)
#   upperm <- 11
#   repi <- 1000
#   delta0 <- 0.01
#   delta1 <- 0.03
#   cores <- 20
#   results <- testMultipleTimesTest_SCLA_bmx2_typeII(configs, upperm, repi, delta0, delta1, cores, agr.cnt=agr.cnt) 
#   write.csv(results[[1]], paste("scla_err_bmx2_typeii_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
#   write.csv(results[[2]], paste("scla_mdist_bmx2_typeii_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
# }

testmx2OnSCLA_typeV  <- function(agr.cnt, d0, d1, upperm=11, repi=1000) {
  configs <- seq(0.2, 1, 0.01)
  configs <- c(0.15, configs)
  delta0 <- d0
  delta1 <- d1
  cores <- 20
  results <- testMultipleTimesTest_SCLA_bmx2_typeV(configs, upperm, repi, delta0, delta1,cores, agr.cnt = agr.cnt) 
  write.csv(results[[1]], paste("test_result/scla_err_bmx2_V_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/scla_mdist_bmx2_V_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste("test_result/scla_type1error_bmx2_V_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste("test_result/scla_type2error_bmx2_V_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}


testmx2OnSCLA_typeVI  <- function(agr.cnt, d0, d1, upperm=11, repi=1000) {
  configs <- seq(0.2, 1, 0.01)
  configs <- c(0.15, configs)
  delta0 <- d0
  delta1 <- d1
  cores <- 20
  results <- testMultipleTimesTest_SCLA_bmx2_typeVI(configs, upperm, repi, delta0, delta1,cores, agr.cnt = agr.cnt) 
  write.csv(results[[1]], paste("test_result/scla_err_bmx2_VI_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/scla_mdist_bmx2_VI_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste("test_result/scla_type1error_bmx2_VI_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste("test_result/scla_type2error_bmx2_VI_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}

testmx2OnSCLA_typeVII  <- function(agr.cnt, d0, d1, upperm=11, repi=1000) {
  configs <- seq(0.2, 1, 0.01)
  configs <- c(0.15, configs)
  delta0 <- d0
  delta1 <- d1
  cores <- 20
  results <- testMultipleTimesTest_SCLA_bmx2_typeVII(configs, upperm, repi, delta0, delta1,cores, agr.cnt = agr.cnt) 
  write.csv(results[[1]], paste("test_result/scla_err_bmx2_VII_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/scla_mdist_bmx2_VII_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste("test_result/scla_type1error_bmx2_VII_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste("test_result/scla_type2error_bmx2_VII_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}


testmx2OnSCLA_typeVIII  <- function(agr.cnt, d0, d1, upperm=11, repi=1000) {
  configs <- seq(0.2, 1, 0.01)
  configs <- c(0.15, configs)
  delta0 <- d0
  delta1 <- d1
  cores <- 20
  results <- testMultipleTimesTest_SCLA_bmx2_typeVIII(configs, upperm, repi, delta0, delta1,cores, agr.cnt = agr.cnt) 
  write.csv(results[[1]], paste("test_result/scla_err_bmx2_VIII_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/scla_mdist_bmx2_VIII_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste("test_result/scla_type1error_bmx2_VIII_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste("test_result/scla_type2error_bmx2_VIII_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}


testmx2OnSCLA_typeIX  <- function(agr.cnt, d0, d1, upperm=11, repi=1000) {
  configs <- seq(0.2, 1, 0.01)
  configs <- c(0.15, configs)
  delta0 <- d0
  delta1 <- d1
  cores <- 20
  results <- testMultipleTimesTest_SCLA_bmx2_typeIX(configs, upperm, repi, delta0, delta1,cores, agr.cnt = agr.cnt) 
  write.csv(results[[1]], paste("test_result/scla_err_bmx2_IX_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/scla_mdist_bmx2_IX_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste("test_result/scla_type1error_bmx2_IX_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste("test_result/scla_type2error_bmx2_IX_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}

testmx2OnSCLA_typeX  <- function(agr.cnt, d0, d1, upperm=11, repi=1000) {
  configs <- seq(0.2, 1, 0.01)
  configs <- c(0.15, configs)
  delta0 <- d0
  delta1 <- d1
  cores <- 20
  results <- testMultipleTimesTest_SCLA_bmx2_typeX(configs, upperm, repi, delta0, delta1,cores, agr.cnt = agr.cnt) 
  write.csv(results[[1]], paste("test_result/scla_err_bmx2_X_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/scla_mdist_bmx2_X_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste("test_result/scla_type1error_bmx2_X_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste("test_result/scla_type2error_bmx2_X_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}

testmx2OnSCLA_general  <- function(agr.cnt, d0, d1, upperm=11, repi=1000, version="I", config.vector = NULL) {
  configs <- seq(0.2, 1, 0.01)
  configs <- c(0.15, configs)
  if(!is.null(config.vector)) {
    configs <- config.vector
  }
  delta0 <- d0
  delta1 <- d1
  results <- testMultipleTimesTest_SCLA_bmx2_type_general(configs, upperm, repi, delta0, delta1, agr.cnt, version) 
  write.csv(results[[1]], paste(paste("test_result/scla_err_bmx2_", version, "_", sep=""),  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste(paste("test_result/scla_mdist_bmx2_", version, "_", sep=""),  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste(paste("test_result/scla_type1error_bmx2_", version, "_", sep=""), "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste(paste("test_result/scla_type2error_bmx2_", version, "_", sep=""), "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}