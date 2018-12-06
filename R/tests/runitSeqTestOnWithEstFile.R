source("./tests/testsCollectDatasets.R", encoding="UTF-8")

testMx2BCVSeqTest_dietterich <- function(upper_m, delta, relative=F, file.name=NULL, alpha = 0.05)  {
  source("./tasks/diet_mx2cv_seq_significant_test.R", encoding="UTF-8")
  results <- c()
  conf_task_dietterich <- list(
    alpha = alpha,
    relative = relative,
    var.est.conf = list(
      name = "var_est_dietterich_mx2cv"
    ),
    delta = delta,
    upper_m = upper_m,
    file_name = file.name,
    lower_m = 3
  )
  mu.matrix <- as.matrix(read.csv(file = file.name, sep=" "))
  row.names(mu.matrix) <- NULL
  colnames(mu.matrix) <- NULL
  sim_count <- nrow(mu.matrix)
  sim_count <- 50
  for(i in 1:sim_count) {
    muv <- mu.matrix[i,]
    if (i%%100 == 0) cat(i,"...")
    conf_task_dietterich$pre_vec <- as.numeric(muv)
    task_config <- diet_mx2cv_seq_significant_test.task_config_validation(conf_task_dietterich)
    result      <- diet_mx2cv_seq_significant_test.perform_task(task_config)
    results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2]))
  }
  m.stop  <- upper_m
  result.table <- c()
  result.h0 <- table(factor(results[which(results[,2]==0),1], levels = conf_task_dietterich$lower_m:m.stop))
  result.table <- rbind(result.table, result.h0)
  result.h1 <- table(factor(results[which(results[,2]==1),1], levels = conf_task_dietterich$lower_m:m.stop))
  result.table <- rbind(result.table, result.h1)
  props <- t(t(rowSums(result.table)/sim_count))
  colnames(props) <- c("prop")
  result.table <- cbind(result.table, props)
  row.names(result.table) <- c("No Reject:", "Reject:")
  # 求取错误率
  m.fixed <- computeExpectStopTime(c(result.h0), c(result.h1), repi =  sim_count)
  conf_task_dietterich <- list(
    alpha = alpha,
    relative = relative,
    var.est.conf = list(
      name = "var_est_dietterich_mx2cv"
    ),
    delta = delta,
    upper_m = upper_m,
    file_name = file.name,
    fixed_m = m.fixed,
    lower_m = 3
  )
  results <- c()
  for(i in 1:sim_count) {
    muv <- mu.matrix[i,]
    if (i%%100 == 0) cat(i,"...")
    conf_task_dietterich$pre_vec <- as.numeric(muv)
    task_config <- diet_mx2cv_seq_significant_test.task_config_validation(conf_task_dietterich)
    result      <- diet_mx2cv_seq_significant_test.perform_task(task_config)
    results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2]))
  }
  m.stop  <- upper_m
  result.table.mfix <- c()
  result.h0.mfix <- table(factor(results[which(results[,2]==0),1], levels = conf_task_dietterich$lower_m:m.stop))
  result.table.mfix <- rbind(result.table.mfix, result.h0.mfix)
  result.h1.mfix <- table(factor(results[which(results[,2]==1),1], levels = conf_task_dietterich$lower_m:m.stop))
  result.table.mfix <- rbind(result.table.mfix, result.h1.mfix)
  props.mfix <- t(t(rowSums(result.table.mfix)/sim_count))
  format.result <- c(result.h0, result.h1, m.fixed, c(props),c(props.mfix))
  colnames(format.result) <- NULL
  names(format.result) <- NULL
  return(format.result)
}



testMx2BCVSeqTest_bmx2cv <- function(upper_m, delta, relative=F, file.name=NULL, est.type="typeI", sim_count=NULL, alpha=0.05, rho1=NULL, rho2=NULL) {
  source("./tasks/mx2cv_seq_significant_test.R", encoding="UTF-8")
  results <- c()
  conf_TASK_BMX2SEQTEST <- list(
    est_type = est.type,
    alpha = alpha,
    relative = relative,
    var.est.conf = list(
      name = "var_est3_mx2cv"
    ),
    delta = delta,
    upper_m = upper_m,
    lower_m = 3
  )
  if(!is.null(rho1) && !is.null(rho2)) {
    conf_TASK_BMX2SEQTEST$rho1 = rho1
    conf_TASK_BMX2SEQTEST$rho2 = rho2
  }
  # mu.matrix <- read.table(file.name)
  mu.matrix <- as.matrix(read.csv(file = file.name, sep=" "))
  row.names(mu.matrix) <- NULL
  colnames(mu.matrix) <- NULL
  if(is.null(sim_count)) {
    sim_count <- nrow(mu.matrix)
  } 
  for(i in 1:sim_count) {
    muv <- mu.matrix[i,]
    if (i%%100 == 0) cat(i,"...")
    conf_TASK_BMX2SEQTEST$pre_vec <- as.numeric(muv)
    task_config <- mx2cv_seq_significant_test.task_config_validation(conf_TASK_BMX2SEQTEST)
    result      <- mx2cv_seq_significant_test.perform_task(task_config)
    results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2]))
  }  
  m.stop  <- upper_m
  result.table <- c()
  result.h0 <- table(factor(results[which(results[,2]==0),1], levels = conf_task_dietterich$lower_m:m.stop))
  result.table <- rbind(result.table, result.h0)
  result.h1 <- table(factor(results[which(results[,2]==1),1], levels = conf_task_dietterich$lower_m:m.stop))
  result.table <- rbind(result.table, result.h1)
  props <- t(t(rowSums(result.table)/sim_count))
  colnames(props) <- c("prop")
  result.table <- cbind(result.table, props)
  row.names(result.table) <- c("No Reject:", "Reject:")
  # 计算错误率
  m.fixed <- computeExpectStopTime(c(result.h0), c(result.h1), repi =  sim_count)
  conf_TASK_BMX2SEQTEST <- list(
    est_type = est.type,
    alpha = alpha,
    relative = relative,
    var.est.conf = list(
      name = "var_est3_mx2cv"
    ),
    delta = delta,
    upper_m = upper_m,
    fixed_m = m.fixed,
    lower_m = 3
  )
  if(!is.null(rho1) && !is.null(rho2)) {
    conf_TASK_BMX2SEQTEST$rho1 = rho1
    conf_TASK_BMX2SEQTEST$rho2 = rho2
  }
  results <- c()
  for(i in 1:sim_count) {
    muv <- mu.matrix[i,]
    if (i%%100 == 0) cat(i,"...")
    conf_TASK_BMX2SEQTEST$pre_vec <- as.numeric(muv)
    task_config <- mx2cv_seq_significant_test.task_config_validation(conf_TASK_BMX2SEQTEST)
    result      <- mx2cv_seq_significant_test.perform_task(task_config)
    results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2],result$test.result[1,5],result$test.result[1,6]))
  }  
  m.stop  <- upper_m
  result.table.mfix <- c()
  result.h0.mfix <- table(factor(results[which(results[,2]==0),1], levels = conf_task_dietterich$lower_m:m.stop))
  result.table.mfix <- rbind(result.table.mfix, result.h0.mfix)
  result.h1.mfix <- table(factor(results[which(results[,2]==1),1], levels = conf_task_dietterich$lower_m:m.stop))
  result.table.mfix <- rbind(result.table.mfix, result.h1.mfix)
  props.mfix <- t(t(rowSums(result.table.mfix)/sim_count))
  format.result <- c(result.h0, result.h1, m.fixed, c(props),c(props.mfix))
  colnames(format.result) <- NULL
  names(format.result) <- NULL
  return(format.result)
}

testWithExternFileParallel_bmx2 <- function(delta.start, delta.end, delta.step, true.value, file.name, upper.m, est.type, dataset.tag, rho1=NULL, rho2=NULL) {
  library(foreach)
  library(doParallel)
  source("./tests/testsCollectDatasets.R", encoding="UTF-8")
  work.directory <- getwd()
  delta.seq <- seq(delta.start, delta.end, delta.step)
  delta.seq <- c(delta.seq, true.value)
  delta.seq <- sort(delta.seq)
  sim.count <- length(delta.seq)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  results <- foreach(i=1:sim.count, .combine = rbind, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    delta <- delta.seq[i]
    result <- testMx2BCVSeqTest_bmx2cv(upper.m, delta,  file.name=file.name, est.type=est.type, rho1=rho1, rho2=rho2)
    c(delta, result)
  }
  write.csv(results, file = paste("est_for_test/", paste("bmx2", dataset.tag, est.type, format(Sys.time(), "%Y%m%d%H%M%S"),sep="_"), sep=""))
}

testWithExternFileParallel_diet <- function(delta.start, delta.end, delta.step, true.value, file.name, upper.m, dataset.tag) {
  library(foreach)
  library(doParallel)
  source("./tests/testsCollectDatasets.R", encoding="UTF-8")
  work.directory <- getwd()
  delta.seq <- seq(delta.start, delta.end, delta.step)
  delta.seq <- c(delta.seq, true.value)
  delta.seq <- sort(delta.seq)
  sim.count <- length(delta.seq)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  results <- foreach(i=1:sim.count, .combine = rbind, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    delta <- delta.seq[i]
    result <- testMx2BCVSeqTest_dietterich(upper.m, delta, file.name=file.name) 
    c(delta, result)
  }
  write.csv(results, file = paste("est_for_test/", paste("diet", dataset.tag, format(Sys.time(), "%Y%m%d%H%M%S"),sep="_"), sep=""))
}