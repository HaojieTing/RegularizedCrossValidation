testMx2CVSeqTestOnHdRegr_bmx2 <- function(upper_m, sim_count, w, delta0, delta1, est.type="typeI", agr.cnt=3) {
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
    dataset.conf = list(
      name = "hdRegrDataFL",
      type = "regression",
      n = 1000,
      d = 300,
      beta = w
    ),
    algorithm2.conf = list(
      name = "LassoModel",
      type = "regression",
      prop = 0.2
    ),
    algorithm1.conf = list (
      name = "lmRidgeModel",
      type = "regression"
    ),
    crossvalidation.conf = list(
      name = "mx2bcv_inc",
      m = 3
    )
  )
  for(i in 1:sim_count) {
    set.seed(123+i)
    if(i%%10 == 0) cat(i,"...\t")
    task_config <- mx2cv_seq_significant_test.task_config_validation(task_config)
    result      <- mx2cv_seq_significant_test.perform_task(task_config)
    results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2],result$test.result[1,5],result$test.result[1,6]))
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
  return(list(result.table, type1error, type2error))
}


testMultipleTimesTestOnHdRegr_bmx2_type_general <- function(ws, upperm, repi, delta0, delta1, agr.cnt, version) {
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
    result <- testMx2CVSeqTestOnHdRegr_bmx2(upperm, repi, w, delta0, delta1, est.type=paste("type", version, sep = ""), agr.cnt=agr.cnt)
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

testmx2OnHdRegr_general  <- function(agr.cnt, d0, d1, upperm=11, repi=1000, version="I", config.vector = NULL) {
  configs <- seq(1, 10, 0.5)
  if(!is.null(config.vector)) {
    configs <- config.vector
  }
  delta0 <- d0
  delta1 <- d1
  results <- testMultipleTimesTestOnHdRegr_bmx2_type_general(configs, upperm, repi, delta0, delta1, agr.cnt, version)
  write.csv(results[[1]], paste(paste("test_result/hdrdgr_err_bmx2_", version, "_", sep=""),  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[2]], paste(paste("test_result/hdrdgr_mdist_bmx2_", version, "_", sep=""),  "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[3]], paste(paste("test_result/hdrdgr_type1error_bmx2_", version, "_", sep=""), "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
  write.csv(results[[4]], paste(paste("test_result/hdrdgr_type2error_bmx2_", version, "_", sep=""), "agr", agr.cnt, "m", upperm, "rep", repi, "d0", delta0, "d1", delta1, "csv", sep="."))
}