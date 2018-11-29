# 测试Letter数据上的序贯检验.
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2018/2/3

testMx2CVSeqTestOnLetter_bmx2 <- function(upper_m, sim_count, w, delta0, delta1, est.type="typeI", agr.cnt=3, seq = T, m.fixed=NULL) {
  source("./tasks/mx2cv_seq_significant_test.R", encoding="UTF-8")
  results <- c()
  task_config <- list(
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
    lower_m = 3,
    fixed_m = m.fixed,
    dataset.conf = list(
      name = "uci_letter",
      type = "classification",
      binarize = TRUE,
      samplingConf = list(
        n = 300
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
      w = w
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
      task_config <- mx2cv_seq_significant_test.task_config_validation(task_config)
      result      <- mx2cv_seq_significant_test.perform_task(task_config)
      results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2],result$test.result[1,5],result$test.result[1,6]))
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
      task_config <- mx2cv_seq_significant_test.task_config_validation(task_config)
      result      <- mx2cv_seq_significant_test.perform_task(task_config)
      c(result$test.result[1,1],result$test.result[1,2],result$test.result[1,5],result$test.result[1,6], result$mu.vec)
    }
    stopCluster(cl)
  }
  result.table <- c()
  m.stop <- upper_m + agr.cnt
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==0),1], levels = task_config$lower_m:m.stop)))
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==1),1], levels = task_config$lower_m:m.stop)))
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==2),1], levels = task_config$lower_m:m.stop)))
  type1error <- mean(results[,3])
  type2error <- mean(results[,4])
  props <- t(t(rowSums(result.table)/sim_count))
  colnames(props) <- c("prop")
  result.table <- cbind(result.table, props)
  row.names(result.table) <- c("accept H0:", "accept H1:", "other:")
  if(seq == F){
    suffix <- w
    repi <- sim_count
    upperm <- upper_m
    write.csv(props, paste(paste("test_result/sp_letter_err_bmx2_", est.type, "_", sep=""),  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
    write.csv(result.table, paste(paste("test_result/sp_letter_mdist_bmx2_", est.type, "_", sep=""),  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
    write.csv(type1error, paste(paste("test_result/sp_letter_type1error_bmx2_", est.type, "_", sep=""), "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
    write.csv(type2error, paste(paste("test_result/sp_letter_type2error_bmx2_", est.type, "_", sep=""), "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
    save(results, file=paste("results_letter", sim_count, m.fixed, "rdata", sep = "."))
  }
  return(list(result.table, type1error, type2error))
}

testMx2CVSeqTestOnLetter_98diet <- function(upper_m, sim_count, w, delta0, delta1, agr.cnt=3, seq = T) {
  source("./tasks/diet_mx2cv_seq_significant_test.R", encoding="UTF-8")
  results <- c()
  task_config <- list(
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
      name = "uci_letter",
      type = "classification",
      binarize = TRUE,
      samplingConf = list(
        n = 300
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
      w = w
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
      task_config <- diet_mx2cv_seq_significant_test.task_config_validation(task_config)
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
      task_config <- diet_mx2cv_seq_significant_test.task_config_validation(task_config)
      result      <- diet_mx2cv_seq_significant_test.perform_task(task_config)
      c(result$test.result[1,1],result$test.result[1,2],result$test.result[1,5],result$test.result[1,6])
    }
    stopCluster(cl)
  }
  m.stop <- upper_m + agr.cnt
  result.table <- c()
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==0),1], levels = task_config$lower_m:m.stop)))
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==1),1], levels = task_config$lower_m:m.stop)))
  result.table <- rbind(result.table, table(factor(results[which(results[,2]==2),1], levels = task_config$lower_m:m.stop)))
  type1error <- mean(results[,3])
  type2error <- mean(results[,4])
  props <- t(t(rowSums(result.table)/sim_count))
  colnames(props) <- c("prop")
  result.table <- cbind(result.table, props)
  row.names(result.table) <- c("accept H0:", "accept H1:", "other:")
  if(seq == F){
    suffix <- w
    repi <- sim_count
    upperm <- upper_m
    write.csv(props, paste("test_result/sp_letter_err_diet_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
    write.csv(result.table, paste("test_result/sp_letter_mdist_diet_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix,"csv", sep="."))
    write.csv(type1error, paste("test_result/sp_letter_type1error_diet_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix,"csv", sep="."))
    write.csv(type2error, paste("test_result/sp_letter_type2error_diet_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix,"csv", sep="."))
  }
  return(list(result.table, type1error, type2error))
}

# testMx2CVSeqTestOnSCLA3_13comb <- function(upper_m, sim_count, w, delta0, delta1, alpha=0.05, beta=0.05, agr.cnt=3) {
#   source("./tasks/combined_mx2cv_seq_t_test.R", encoding="UTF-8")
#   results <- c()
#   task_config <- list(
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
#       name = "uci_letter",
#       type = "classification",
#       binarize = TRUE,
#       samplingConf = list(
#         n = 300
#       )
#     ),
#     algorithm2.conf = list(
#       name = "classificationTree",
#       type = "classification",
#       method = "class"
#     ),
#     algorithm1.conf = list(
#       name = "firstNearestNeighborhood",
#       type = "classification",
#       w = w
#     ),
#     crossvalidation.conf = list(
#       name = "mx2bcv_inc",
#       m = 3
#     )
#   )
#   for(i in 1:sim_count) {
#     set.seed(123+i)
#     if(i%%10 == 0) cat(i,"...\t")
#     task_config <- combined_mx2cv_seq_t_test.task_config_validation(task_config)
#     result      <- combined_mx2cv_seq_t_test.perform_task(task_config)
#     results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2]))
#   }
#   cat("\n")
#   m.stop <- upper_m + agr.cnt
#   result.table <- c()
#   result.table <- rbind(result.table, table(factor(results[which(results[,2]==0),1], levels = task_config$lower_m:m.stop)))
#   result.table <- rbind(result.table, table(factor(results[which(results[,2]==1),1], levels = task_config$lower_m:m.stop)))
#   result.table <- rbind(result.table, table(factor(results[which(results[,2]==2),1], levels = task_config$lower_m:m.stop)))
#   props <- t(t(rowSums(result.table)/sim_count))
#   colnames(props) <- c("prop")
#   result.table <- cbind(result.table, props)
#   row.names(result.table) <- c("accept H0:", "accept H1:", "other:")
#   return(result.table)
# }

# =================== Test Multiple Times Tests ==========================

testMultipleTimesTestOnLetter_98diet <- function(ws, upperm, repi, delta0, delta1, cores, agr.cnt) {
  results <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  w.length <- length(ws)
  work.directory <- getwd()
  results <- foreach(idx = 1: w.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    w <- ws[idx]
    result <- testMx2CVSeqTestOnLetter_98diet(upperm, repi, w, delta0, delta1, agr.cnt=3)
    result.first <- result[[1]]
    prop.col <- result.first[,ncol(result.first)]
    count.cols <- result.first[,1:(ncol(result.first)-1)]
    count.vec <- c(t(count.cols))
    type1error <- result[[2]]
    type2error <- result[[3]]
    list(prop.col, count.vec,type1error,type2error )
  }
  stopCluster(cl)
  m.stop <- upperm + agr.cnt
  props <- do.call(rbind,lapply(results, "[[", 1))
  occus <- do.call(rbind,lapply(results, "[[", 2))
  props <- t(props)
  type1errors <- do.call(cbind,lapply(results, "[[", 3))
  type2errors <- do.call(cbind,lapply(results, "[[", 4))
  colnames(props) <- ws
  rownames(occus) <- ws
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus,type1errors,type2errors))
}

# testMultipleTimesTestOnLetter_13comb <- function(ws, upperm, repi, delta0, delta1, alpha=0.05, beta=0.05, cores=20, agr.cnt=3) {
#   results <- c()
#   library(foreach)
#   library(doParallel)
#   cl <- makeCluster(cores)
#   registerDoParallel(cl)
#   ws.length <- length(ws)
#   results <- foreach(idx = 1: ws.length, .export = ls(.GlobalEnv)) %dopar% {
#     w <- ws[idx]
#     result <- testMx2CVSeqTestOnSCLA3_13comb(upperm, repi, w, delta0, delta1, alpha, beta, agr.cnt=agr.cnt)
#     prop.col <- result[,ncol(result)]
#     count.cols <- result[,1:(ncol(result)-1)]
#     count.vec <- c(t(count.cols))
#     list(prop.col, count.vec)
#   }
#   stopCluster(cl)
#   props <- do.call(rbind,lapply(results, "[[", 1))
#   occus <- do.call(rbind,lapply(results, "[[", 2))
#   props <- t(props)
#   colnames(props) <- ws
#   rownames(occus) <- ws
#   m.stop <- upperm + agr.cnt
#   print(m.stop)
#   colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
#   return(list(props, occus))
# }

testMultipleTimesTestOnLetter_bmx2 <- function(ws, upperm, repi, delta0, delta1, cores, agr.cnt) {
  results <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  ws.length <- length(ws)
  work.directory <- getwd()
  results <- foreach(idx = 1: ws.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    w <- ws[idx]
    result <- testMx2CVSeqTestOnLetter_bmx2(upperm, repi, w, delta0, delta1, est.type="typeI", agr.cnt=agr.cnt)
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
  colnames(props) <- ws
  rownames(occus) <- ws
  m.stop <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus,type1errors,type2errors))
}

testMultipleTimesTestOnLetter_14bmx2 <- function(ws, upperm, repi, delta0, delta1, cores, agr.cnt) {
  results <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  ws.length <- length(ws)
  work.directory <- getwd()
  results <- foreach(idx = 1: ws.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    w <- ws[idx]
    result <- testMx2CVSeqTestOnLetter_bmx2(upperm, repi, w, delta0, delta1, est.type="14est", agr.cnt=agr.cnt)
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
  colnames(props) <- ws
  rownames(occus) <- ws
  m.stop <- upperm + agr.cnt
  print(m.stop)
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus, type1errors, type2errors))
}


testMultipleTimesTestOnLetter_bmx2_typeV <- function(ws, upperm, repi, delta0, delta1, cores, agr.cnt) {
  results <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  ws.length <- length(ws)
  work.directory <- getwd()
  results <- foreach(idx = 1: ws.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    w <- ws[idx]
    result <- testMx2CVSeqTestOnLetter_bmx2(upperm, repi, w, delta0, delta1, est.type="typeV", agr.cnt=agr.cnt)
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
  colnames(props) <- ws
  rownames(occus) <- ws
  m.stop <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus,type1errors,type2errors))
}

testMultipleTimesTestOnLetter_bmx2_typeVI <- function(ws, upperm, repi, delta0, delta1, cores, agr.cnt) {
  results <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  ws.length <- length(ws)
  work.directory <- getwd()
  results <- foreach(idx = 1: ws.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    w <- ws[idx]
    result <- testMx2CVSeqTestOnLetter_bmx2(upperm, repi, w, delta0, delta1, est.type="typeVI", agr.cnt=agr.cnt)
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
  colnames(props) <- ws
  rownames(occus) <- ws
  m.stop <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus,type1errors,type2errors))
}


testMultipleTimesTestOnLetter_bmx2_typeVII <- function(ws, upperm, repi, delta0, delta1, cores, agr.cnt) {
  results <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  ws.length <- length(ws)
  work.directory <- getwd()
  results <- foreach(idx = 1: ws.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    w <- ws[idx]
    result <- testMx2CVSeqTestOnLetter_bmx2(upperm, repi, w, delta0, delta1, est.type="typeVII", agr.cnt=agr.cnt)
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
  colnames(props) <- ws
  rownames(occus) <- ws
  m.stop <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus,type1errors,type2errors))
}


testMultipleTimesTestOnLetter_bmx2_typeVIII <- function(ws, upperm, repi, delta0, delta1, cores, agr.cnt) {
  results <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  ws.length <- length(ws)
  work.directory <- getwd()
  results <- foreach(idx = 1: ws.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    w <- ws[idx]
    result <- testMx2CVSeqTestOnLetter_bmx2(upperm, repi, w, delta0, delta1, est.type="typeVIII", agr.cnt=agr.cnt)
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
  colnames(props) <- ws
  rownames(occus) <- ws
  m.stop <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus,type1errors,type2errors))
}


testMultipleTimesTestOnLetter_bmx2_typeIX <- function(ws, upperm, repi, delta0, delta1, cores, agr.cnt) {
  results <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  ws.length <- length(ws)
  work.directory <- getwd()
  results <- foreach(idx = 1: ws.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    w <- ws[idx]
    result <- testMx2CVSeqTestOnLetter_bmx2(upperm, repi, w, delta0, delta1, est.type="typeIX", agr.cnt=agr.cnt)
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
  colnames(props) <- ws
  rownames(occus) <- ws
  m.stop <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus,type1errors,type2errors))
}


testMultipleTimesTestOnLetter_bmx2_typeX <- function(ws, upperm, repi, delta0, delta1, cores, agr.cnt) {
  results <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  ws.length <- length(ws)
  work.directory <- getwd()
  results <- foreach(idx = 1: ws.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    w <- ws[idx]
    result <- testMx2CVSeqTestOnLetter_bmx2(upperm, repi, w, delta0, delta1, est.type="typeX", agr.cnt=agr.cnt)
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
  colnames(props) <- ws
  rownames(occus) <- ws
  m.stop <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus,type1errors,type2errors))
}

testMultipleTimesTestOnLetter_bmx2_type_general <- function(ws, upperm, repi, delta0, delta1, agr.cnt, version) {
  results <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  ws.length <- length(ws)
  work.directory <- getwd()
  results <- foreach(idx = 1: ws.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    w <- ws[idx]
    result <- testMx2CVSeqTestOnLetter_bmx2(upperm, repi, w, delta0, delta1, est.type=paste("type", version, sep = ""), agr.cnt=agr.cnt)
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
  colnames(props) <- ws
  rownames(occus) <- ws
  m.stop <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus,type1errors,type2errors))
}

# testMultipleTimesTestOnLetter_typeii <- function(ws, upperm, repi, delta0, delta1, cores, agr.cnt) {
#   results <- c()
#   library(foreach)
#   library(doParallel)
#   cl <- makeCluster(cores)
#   registerDoParallel(cl)
#   ws.length <- length(ws)
#   results <- foreach(idx = 1: ws.length, .export = ls(.GlobalEnv)) %dopar% {
#     w <- ws[idx]
#     result <- testMx2CVSeqTestOnLetter_bmx2(upperm, repi, w, delta0, delta1, est.type="typeII", agr.cnt=agr.cnt)
#     prop.col <- result[,ncol(result)]
#     count.cols <- result[,1:(ncol(result)-1)]
#     count.vec <- c(t(count.cols))
#     list(prop.col, count.vec)
#   }
#   stopCluster(cl)
#   props <- do.call(rbind,lapply(results, "[[", 1))
#   occus <- do.call(rbind,lapply(results, "[[", 2))
#   props <- t(props)
#   colnames(props) <- ws
#   rownames(occus) <- ws
#   m.stop <- upperm + agr.cnt
#   print(m.stop)
#   colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
#   return(list(props, occus))
# }

#====================== Test invoker function =====================

testDietOnLetter  <- function(agr.cnt, d0, d1, upperm=11, repi=1000, config.vector = NULL) {
  configs <- seq(1, 100, 1)
  if(!is.null(config.vector)) {
    configs <- config.vector
  }
  delta0 <- d0
  delta1 <- d1
  cores <- 20
  results <- testMultipleTimesTestOnLetter_98diet(configs, upperm, repi, delta0, delta1, cores, agr.cnt=agr.cnt)
  write.csv(results[[1]], paste("test_result/letter_err_dietext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/letter_mdist_dietext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste("test_result/letter_type1error_dietext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste("test_result/letter_type2error_dietext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}

# testCombOnLetter  <- function(d0, d1, agr.cnt=3, repi=1000) {
#   configs <- seq(1, 100, 1)
#   upperm <- 11
#   repi <- repi
#   delta0 <- d0
#   delta1 <- d1
#   alpha <- 0.05
#   beta <- 0.05
#   cores <- 20
#   results <- testMultipleTimesTestOnLetter_13comb(configs, upperm, repi, delta0, delta1, alpha=alpha, beta=beta, cores=20, agr.cnt=agr.cnt)
#   write.csv(results[[1]], paste("test_result/letter_err_comb_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, alpha, beta, "csv", sep="."))
#   write.csv(results[[2]], paste("test_result/letter_mdist_comb_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1,alpha, beta,  "csv", sep="."))
# }
# 
# testCombOnLetter_largem  <- function(d0, d1, agr.cnt=3, repi=1000) {
#   configs <- seq(1, 100, 1)
#   upperm <- 50
#   repi <- repi
#   delta0 <- d0
#   delta1 <- d1
#   alpha <- 0.05
#   beta <- 0.05
#   cores <- 20
#   results <- testMultipleTimesTestOnLetter_13comb(configs, upperm, repi, delta0, delta1, alpha=alpha, beta=beta, cores=20, agr.cnt=agr.cnt)
#   write.csv(results[[1]], paste("test_result/letter_err_comb_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, alpha, beta, "csv", sep="."))
#   write.csv(results[[2]], paste("test_result/letter_mdist_comb_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, alpha, beta,  "csv", sep="."))
# }

testmx2OnLetter  <- function(agr.cnt, d0, d1, upperm=11, repi=1000, config.vector = NULL) {
  configs <- seq(1, 100, 1)
  if(!is.null(config.vector)) {
    configs <- config.vector
  }
  delta0 <- d0
  delta1 <- d1
  cores <- 20
  results <- testMultipleTimesTestOnLetter_bmx2(configs, upperm, repi, delta0, delta1, cores, agr.cnt=agr.cnt)
  write.csv(results[[1]], paste("test_result/letter_err_bmx2ext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/letter_mdist_bmx2ext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste("test_result/letter_type1error_bmx2ext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste("test_result/letter_type2error_bmx2ext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}

testmx2OnLetter14  <- function(agr.cnt, d0, d1, upperm=11, repi=1000, config.vector = NULL) {
  configs <- seq(1, 100, 1)
  if(!is.null(config.vector)) {
    configs <- config.vector
  }
  delta0 <- d0
  delta1 <- d1
  cores <- 20
  results <- testMultipleTimesTestOnLetter_14bmx2(configs, upperm, repi, delta0, delta1, cores, agr.cnt=agr.cnt)
  write.csv(results[[1]], paste("test_result/letter_err_bmx214ext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/letter_mdist_bmx214ext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste("test_result/letter_type1error_bmx214ext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste("test_result/letter_type2error_bmx214ext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}



testmx2OnLetter_typeV  <- function(agr.cnt, d0, d1, upperm=11, repi=1000, config.vector = NULL) {
  configs <- seq(1, 100, 1)
  if(!is.null(config.vector)) {
    configs <- config.vector
  }
  delta0 <- d0
  delta1 <- d1
  cores <- 20
  results <- testMultipleTimesTestOnLetter_bmx2_typeV(configs, upperm, repi, delta0, delta1, cores, agr.cnt=agr.cnt)
  write.csv(results[[1]], paste("test_result/letter_err_bmx2_Vext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/letter_mdist_bmx2_Vext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste("test_result/letter_type1error_bmx2_Vext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste("test_result/letter_type2error_bmx2_Vext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}


testmx2OnLetter_typeVI  <- function(agr.cnt, d0, d1, upperm=11, repi=1000, config.vector = NULL) {
  configs <- seq(1, 100, 1)
  if(!is.null(config.vector)) {
    configs <- config.vector
  }
  delta0 <- d0
  delta1 <- d1
  cores <- 20
  results <- testMultipleTimesTestOnLetter_bmx2_typeVI(configs, upperm, repi, delta0, delta1, cores, agr.cnt=agr.cnt)
  write.csv(results[[1]], paste("test_result/letter_err_bmx2_VIext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/letter_mdist_bmx2_VIext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste("test_result/letter_type1error_bmx2_VIext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste("test_result/letter_type2error_bmx2_VIext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}


testmx2OnLetter_typeVII  <- function(agr.cnt, d0, d1, upperm=11, repi=1000, config.vector = NULL) {
  configs <- seq(1, 100, 1)
  if(!is.null(config.vector)) {
    configs <- config.vector
  }
  delta0 <- d0
  delta1 <- d1
  cores <- 20
  results <- testMultipleTimesTestOnLetter_bmx2_typeVII(configs, upperm, repi, delta0, delta1, cores, agr.cnt=agr.cnt)
  write.csv(results[[1]], paste("test_result/letter_err_bmx2_VIIext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/letter_mdist_bmx2_VIIext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste("test_result/letter_type1error_bmx2_VIIext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste("test_result/letter_type2error_bmx2_VIIext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}


testmx2OnLetter_typeVIII  <- function(agr.cnt, d0, d1, upperm=11, repi=1000, config.vector = NULL) {
  configs <- seq(1, 100, 1)
  if(!is.null(config.vector)) {
    configs <- config.vector
  }
  delta0 <- d0
  delta1 <- d1
  cores <- 20
  results <- testMultipleTimesTestOnLetter_bmx2_typeVIII(configs, upperm, repi, delta0, delta1, cores, agr.cnt=agr.cnt)
  write.csv(results[[1]], paste("test_result/letter_err_bmx2_VIIIext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/letter_mdist_bmx2_VIIIext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste("test_result/letter_type1error_bmx2_VIIIext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste("test_result/letter_type2error_bmx2_VIIIext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}


testmx2OnLetter_typeIX  <- function(agr.cnt, d0, d1, upperm=11, repi=1000, config.vector = NULL) {
  configs <- seq(1, 100, 1)
  if(!is.null(config.vector)) {
    configs <- config.vector
  }
  delta0 <- d0
  delta1 <- d1
  cores <- 20
  results <- testMultipleTimesTestOnLetter_bmx2_typeIX(configs, upperm, repi, delta0, delta1, cores, agr.cnt=agr.cnt)
  write.csv(results[[1]], paste("test_result/letter_err_bmx2_IXext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/letter_mdist_bmx2_IXext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste("test_result/letter_type1error_bmx2_IXext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste("test_result/letter_type2error_bmx2_IXext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}
# testmx2OnLetter_typeII  <- function(d0, d1, agr.cnt=3, repi=1000) {
#   configs <- seq(1, 100, 1)
#   upperm <- 11
#   repi <- repi
#   delta0 <- d0
#   delta1 <- d1
#   cores <- 20
#   results <- testMultipleTimesTestOnLetter_typeii(configs, upperm, repi, delta0, delta1, cores, agr.cnt=agr.cnt)
#   write.csv(results[[1]], paste("test_result/letter_err_bmx2_typeii_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
#   write.csv(results[[2]], paste("test_result/letter_mdist_bmx2_typeii_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
# }
testmx2OnLetter_typeX  <- function(agr.cnt, d0, d1, upperm=11, repi=1000, config.vector = NULL) {
  configs <- seq(1, 100, 1)
  if(!is.null(config.vector)) {
    configs <- config.vector
  }
  delta0 <- d0
  delta1 <- d1
  cores <- 20
  results <- testMultipleTimesTestOnLetter_bmx2_typeX(configs, upperm, repi, delta0, delta1, cores, agr.cnt=agr.cnt)
  write.csv(results[[1]], paste("test_result/letter_err_bmx2_Xext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/letter_mdist_bmx2_Xext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste("test_result/letter_type1error_bmx2_Xext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste("test_result/letter_type2error_bmx2_Xext_", "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}


testmx2OnLetter_general  <- function(agr.cnt, d0, d1, upperm=11, repi=1000, version="I", config.vector = NULL) {
  configs <- seq(1, 100, 1)
  if(!is.null(config.vector)) {
    configs <- config.vector
  }
  delta0 <- d0
  delta1 <- d1
  results <- testMultipleTimesTestOnLetter_bmx2_type_general(configs, upperm, repi, delta0, delta1, agr.cnt, version)
  write.csv(results[[1]], paste(paste("test_result/letter_err_bmx2ext_", version, "_", sep=""),  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste(paste("test_result/letter_mdist_bmx2ext_", version, "_", sep=""),  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste(paste("test_result/letter_type1error_bmx2ext_", version, "_", sep=""), "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste(paste("test_result/letter_type2error_bmx2ext_", version, "_", sep=""), "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}