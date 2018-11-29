testMx2BCVSeqTestOnSREG_dietterich <- function(upper_m, sim_count, param, delta0, delta1, agr.cnt=3, relative=F, seq=T, n.size= 2000, m.fixed = NULL)  {
  #
  #
  source("./tasks/diet_mx2cv_seq_significant_test.R", encoding="UTF-8")
  results <- c()
  conf_task_dietterich <- list(
    alpha = 0.05,
    beta  = 0.05,
    agree_cnt = agr.cnt,
    relative = relative,
    var.est.conf = list(
      name = "var_est_dietterich_mx2cv"
    ),
    delta_0 = delta0,
    delta_1 = delta1,
    upper_m = upper_m,
    fixed_m = m.fixed,
    lower_m = 3,
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
  if(seq == T) {
    for(i in 1:sim_count) {
    #  set.seed(123+i)
      #if(i%%10 == 0) cat(i,"...\t")
      task_config <- diet_mx2cv_seq_significant_test.task_config_validation(conf_task_dietterich)
      result      <- diet_mx2cv_seq_significant_test.perform_task(task_config)
      results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2],result$test.result[1,5],result$test.result[1,6]))
    }
  } else {
    library(foreach)
    library(doParallel)
    cl <- makeCluster(20)
    registerDoParallel(cl)
    work.directory <- getwd()
    results <- foreach(i = 1:sim_count, .combine = rbind, .export = ls(.GlobalEnv)) %dopar% {
      setwd(work.directory)
 #     set.seed(123+i)
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
    suffix <- param
    repi <- sim_count
    upperm <- upper_m
    write.csv(props, paste("test_result/sp_sreg_err_diet_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
    write.csv(result.table, paste("test_result/sp_sreg_mdist_diet_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix,"csv", sep="."))
    write.csv(type1error, paste("test_result/sp_sreg_type1error_diet_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix,"csv", sep="."))
    write.csv(type2error, paste("test_result/sp_sreg_type2error_diet_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix,"csv", sep="."))
  }
  return(list(result.table, type1error, type2error))
}


testMx2BCVSeqTestOnSREG_bmx2cv <- function(upper_m, sim_count, param, delta0, delta1, est.type="typeI", agr.cnt=3, metric.conf=NULL,relative=F, seq =T, n.size= 2000, m.fixed = NULL ) {
  source("./tasks/mx2cv_seq_significant_test.R", encoding="UTF-8")
  results <- c()
  conf_TASK_BMX2SEQTEST <- list(
    est_type = est.type,
    alpha = 0.05,
    beta  = 0.05,
    agree_cnt = agr.cnt,
    relative = relative,
    var.est.conf = list(
      name = "var_est3_mx2cv"
    ),
    metric.conf = metric.conf,
    delta_0 = delta0,
    delta_1 = delta1,
    upper_m = upper_m,
    fixed_m = m.fixed,
    lower_m = 3,
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
  if(seq == T) {
      for(i in 1:sim_count) {
        #if(i%%10 == 0) cat(i,"...\t")
        task_config <- mx2cv_seq_significant_test.task_config_validation(conf_TASK_BMX2SEQTEST)
        result      <- mx2cv_seq_significant_test.perform_task(task_config)
        results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2],result$test.result[1,5],result$test.result[1,6]))
      }  
    }else {
    library(foreach)
    library(doParallel)
    cl <- makeCluster(20)
    registerDoParallel(cl)
    work.directory <- getwd()
    results <- foreach(i = 1:sim_count, .combine = rbind, .export = ls(.GlobalEnv)) %dopar% {
      setwd(work.directory)
      task_config <- mx2cv_seq_significant_test.task_config_validation(conf_TASK_BMX2SEQTEST)
      result      <- mx2cv_seq_significant_test.perform_task(task_config)
      c(result$test.result[1,1],result$test.result[1,2],result$test.result[1,5],result$test.result[1,6])
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
    suffix <- param
    repi <- sim_count
    upperm <- upper_m
    write.csv(props, paste(paste("test_result/sp_sreg_err_bmx2_", est.type, "_", sep=""),  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
    write.csv(result.table, paste(paste("test_result/sp_sreg_mdist_bmx2_", est.type, "_", sep=""),  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
    write.csv(type1error, paste(paste("test_result/sp_sreg_type1error_bmx2_", est.type, "_", sep=""), "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
    write.csv(type2error, paste(paste("test_result/sp_sreg_type2error_bmx2_", est.type, "_", sep=""), "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
  }
  cat(type1error, type2error, sep="\t")
  return(list(result.table, type1error, type2error))
}

# testMx2BCVSeqTestOnSREG_combinedmx2cv <- function(upper_m, sim_count, param, delta0, delta1, agr.cnt=3) {
#   source("./tasks/combined_mx2cv_seq_t_test.R", encoding="UTF-8")
#   results <- c()
#   conf_task_combined <- list(
#     alpha = 0.05,
#     beta  = 0.05,
#     agree_cnt = agr.cnt,
#     var.est.conf = list(
#       name = "var_est_dietterich_mx2cv"
#     ),
#     delta_0 = delta0,
#     delta_1 = delta1,
#     upper_m = upper_m,
#     lower_m = 3,
#     dataset.conf = list(
#       name = "SimpleNormalDataSet",
#       type = "regression",
#       shortcut_name = "infor_for_gene_err",
#       mu_scale_y = param
#     ),
#     algorithm2.conf = list(
#       name = "linearModel",
#       type = "regression",
#       no_intercept = FALSE
#     ),
#     algorithm1.conf = list (
#       name = "simpleMeanRegression",
#       type = "regression"
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
#   m.stop  <- upper_m + agr.cnt
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

testMultipleTimesTest_SREG_diet <- function(param1v, upperm, repi, delta0, delta1, agr.cnt, relative=F, n.size= 2000) {
  results <- c()
  occu.vec <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  param1v.length <- length(param1v)
  work.directory <- getwd()
  results <- foreach(idx = 1: param1v.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    param <- param1v[idx]
    result <- testMx2BCVSeqTestOnSREG_dietterich(upperm, repi, param, delta0, delta1, agr.cnt, relative = relative, n.size= n.size)
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
  colnames(props) <- param1v
  rownames(occus) <- param1v
  m.stop  <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus,type1errors,type2errors))
}

# testMultipleTimesTest_SREG_comb <- function(param1v, upperm, repi, delta0, delta1, agr.cnt,cores) {
#   results <- c()
#   occu.vec <- c()
#   library(foreach)
#   library(doParallel)
#   cl <- makeCluster(cores)
#   registerDoParallel(cl)
#   param1v.length <- length(param1v)
#   results <- foreach(idx = 1: param1v.length, .export = ls(.GlobalEnv)) %dopar% {
#     param <- param1v[idx]
#     result <- testMx2BCVSeqTestOnSREG_combinedmx2cv(upperm, repi, param, delta0, delta1, agr.cnt = agr.cnt)
#     prop.col <- result[,ncol(result)]
#     count.cols <- result[,1:(ncol(result)-1)]
#     count.vec <- c(t(count.cols))
#     list(prop.col, count.vec)
#   }
#   stopCluster(cl)
#   props <- do.call(rbind,lapply(results, "[[", 1))
#   occus <- do.call(rbind,lapply(results, "[[", 2))
#   props <- t(props)
#   colnames(props) <- param1v
#   rownames(occus) <- param1v
#   m.stop  <- upperm + agr.cnt
#   colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
#   return(list(props, occus))
# }

testMultipleTimesTest_SREG_bmx2cv_type_general <- function(param1v, upperm, repi, delta0, delta1, agr.cnt, version, metric.conf = NULL, relative = F, n.size= 2000) {
  results <- c()
  occu.vec <- c()
  library(foreach)
  library(doParallel)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  param1v.length <- length(param1v)
  work.directory <- getwd()
  results <- foreach(idx = 1: param1v.length, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    param <- param1v[idx]
    result <- testMx2BCVSeqTestOnSREG_bmx2cv(upperm, repi, param, delta0, delta1, est.type = paste("type", version, sep = ""), agr.cnt = agr.cnt, metric.conf = metric.conf,  relative = relative, n.size = n.size)
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
  type1errors <- do.call(cbind,lapply(results, "[[", 3))
  type2errors <- do.call(cbind,lapply(results, "[[", 4))
  props <- t(props)
  colnames(props) <- param1v
  rownames(occus) <- param1v
  m.stop  <- upperm + agr.cnt
  colnames(occus) <- c(paste("accH0",seq(3,m.stop),sep="_"),paste("accH1",seq(3,m.stop),sep="_"),paste("noOne",seq(3,m.stop),sep="_"))
  return(list(props, occus, type1errors, type2errors))
}

#==============================================================================================================

testDietOnSREG  <- function(agr.cnt, d0, d1, upperm=11, repi=1000, param.vector = NULL, relative = F, n.size= 2000) {
  param1v <- seq(1.5, 6.5,0.1)
  if(!is.null(param.vector)) {
    param1v <- param.vector
  }
  upperm <- upperm
  repi <- repi
  delta0 <- d0
  delta1 <- d1
  agr.cnt <- agr.cnt
  suffix <- ""
  if(relative == T) {
    suffix = paste(suffix, "relative", sep="_")
  }
  if(n.size != 2000) {
    suffix = paste(suffix, n.size, sep="_")
  }
  results <- testMultipleTimesTest_SREG_diet(param1v, upperm, repi, delta0, delta1, agr.cnt = agr.cnt, relative = relative, n.size = n.size) 
  write.csv(results[[1]], paste("test_result/sreg_err_diet_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
  write.csv(results[[2]], paste("test_result/sreg_mdist_diet_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix,"csv", sep="."))
  write.csv(results[[3]], paste("test_result/sreg_type1error_diet_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix,"csv", sep="."))
  write.csv(results[[4]], paste("test_result/sreg_type2error_diet_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix,"csv", sep="."))
}

# testCombOnSREG  <- function(agr.cnt, d0, d1, upperm=11, repi=1000) {
#   param1v <- seq(1.5, 6.5,0.1)
#   upperm <- upperm
#   repi <- repi
#   delta0 <- d0
#   delta1 <- d1
#   agr.cnt <- agr.cnt
#   results <- testMultipleTimesTest_SREG_comb(param1v, upperm, repi, delta0, delta1, agr.cnt = agr.cnt, cores = 20) 
#   write.csv(results[[1]], paste("test_result/sreg_err_comb_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
#   write.csv(results[[2]], paste("test_result/sreg_mdist_comb_",  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
# }

testmx2OnSREG_general <- function(agr.cnt, d0, d1, upperm=11, repi=1000, version="I", param.vector = NULL, metric.conf = NULL, relative = F, n.size= 2000) {
  param1v <- seq(1.5, 6.5, 0.1)
  if(!is.null(param.vector)) {
    param1v <- param.vector
  }
  upperm <- upperm
  repi <- repi
  delta0 <- d0
  delta1 <- d1
  agr.cnt <- agr.cnt
  results <- testMultipleTimesTest_SREG_bmx2cv_type_general(param1v, upperm, repi, delta0, delta1, agr.cnt = agr.cnt, version = version, metric.conf = metric.conf, relative = relative, n.size = n.size) 
  suffix <- ""
  if(relative == T) {
    suffix = paste(suffix, "relative", sep=".")
  }
  if(n.size != 2000) {
    suffix = paste(suffix, n.size, sep="_")
  }
  write.csv(results[[1]], paste(paste("test_result/sreg_err_bmx2_", version, "_", sep=""),  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
  write.csv(results[[2]], paste(paste("test_result/sreg_mdist_bmx2_", version, "_", sep=""),  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
  write.csv(results[[3]], paste(paste("test_result/sreg_type1error_bmx2_", version, "_", sep=""), "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
  write.csv(results[[4]], paste(paste("test_result/sreg_type2error_bmx2_", version, "_", sep=""), "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, suffix, "csv", sep="."))
  return(results)
}