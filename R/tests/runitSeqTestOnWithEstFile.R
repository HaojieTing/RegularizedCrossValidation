source("./tests/testsCollectDatasets.R", encoding="UTF-8")

testMx2BCVSeqTest_dietterich <- function(upper_m, delta0, delta1, agr.cnt=1, relative=F, file.name=NULL, alpha = 0.05, beta=0.05)  {
  source("./tasks/diet_mx2cv_seq_significant_test.R", encoding="UTF-8")
  results <- c()
  conf_task_dietterich <- list(
    alpha = alpha,
    beta  = beta,
    agree_cnt = agr.cnt,
    relative = relative,
    var.est.conf = list(
      name = "var_est_dietterich_mx2cv"
    ),
    delta_0 = delta0,
    delta_1 = delta1,
    upper_m = upper_m,
    file_name = file.name,
    lower_m = 3
  )
  mu.matrix <- as.matrix(read.csv(file = file.name, sep=" "))
  row.names(mu.matrix) <- NULL
  colnames(mu.matrix) <- NULL
  sim_count <- nrow(mu.matrix)
  for(i in 1:sim_count) {
    muv <- mu.matrix[i,]
    if (i%%100 == 0) cat(i,"...")
    conf_task_dietterich$pre_vec <- as.numeric(muv)
    task_config <- diet_mx2cv_seq_significant_test.task_config_validation(conf_task_dietterich)
    result      <- diet_mx2cv_seq_significant_test.perform_task(task_config)
    results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2],result$test.result[1,5],result$test.result[1,6]))
  }
  m.stop  <- upper_m + agr.cnt-1
  result.table <- c()
  result.h0 <- table(factor(results[which(results[,2]==0),1], levels = conf_task_dietterich$lower_m:m.stop))
  result.table <- rbind(result.table, result.h0)
  result.h1 <- table(factor(results[which(results[,2]==1),1], levels = conf_task_dietterich$lower_m:m.stop))
  result.table <- rbind(result.table, result.h1)
  result.no <- table(factor(results[which(results[,2]==2),1], levels = conf_task_dietterich$lower_m:m.stop))
  result.table <- rbind(result.table, result.no)
  props <- t(t(rowSums(result.table)/sim_count))
  colnames(props) <- c("prop")
  result.table <- cbind(result.table, props)
  row.names(result.table) <- c("accept H0:", "accept H1:", "other:")
  # 求取错误率
  m.fixed <- computeExpectStopTime(c(result.h0), c(result.h1), c(result.no), sim_count)
  conf_task_dietterich <- list(
    alpha = alpha,
    beta  = beta,
    agree_cnt = agr.cnt,
    relative = relative,
    var.est.conf = list(
      name = "var_est_dietterich_mx2cv"
    ),
    delta_0 = delta0,
    delta_1 = delta1,
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
    results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2],result$test.result[1,5],result$test.result[1,6]))
  }
  m.stop  <- upper_m + agr.cnt-1
  result.table.mfix <- c()
  result.h0.mfix <- table(factor(results[which(results[,2]==0),1], levels = conf_task_dietterich$lower_m:m.stop))
  result.table.mfix <- rbind(result.table.mfix, result.h0.mfix)
  result.h1.mfix <- table(factor(results[which(results[,2]==1),1], levels = conf_task_dietterich$lower_m:m.stop))
  result.table.mfix <- rbind(result.table.mfix, result.h1.mfix)
  result.no.mfix <- table(factor(results[which(results[,2]==2),1], levels = conf_task_dietterich$lower_m:m.stop))
  result.table.mfix <- rbind(result.table.mfix, result.no.mfix)
  props.mfix <- t(t(rowSums(result.table.mfix)/sim_count))
  type1error <- mean(results[,3])
  type2error <- mean(results[,4])
  format.result <- c(result.h0, result.h1, result.no, m.fixed, c(props),c(props.mfix), type1error, type2error)
  colnames(format.result) <- NULL
  names(format.result) <- NULL
  return(format.result)
}



testMx2BCVSeqTest_bmx2cv <- function(upper_m, delta0, delta1, agr.cnt=1, relative=F, file.name=NULL, est.type="typeI", sim_count=NULL, alpha=0.05, beta=0.05, rho1=NULL, rho2=NULL) {
  source("./tasks/mx2cv_seq_significant_test.R", encoding="UTF-8")
  results <- c()
  conf_TASK_BMX2SEQTEST <- list(
    est_type = est.type,
    alpha = alpha,
    beta  = beta,
    agree_cnt = agr.cnt,
    relative = relative,
    var.est.conf = list(
      name = "var_est3_mx2cv"
    ),
    delta_0 = delta0,
    delta_1 = delta1,
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
    results     <- rbind(results, c(result$test.result[1,1],result$test.result[1,2],result$test.result[1,5],result$test.result[1,6]))
  }  
  m.stop  <- upper_m + agr.cnt-1
  result.table <- c()
  result.h0 <- table(factor(results[which(results[,2]==0),1], levels = conf_TASK_BMX2SEQTEST$lower_m:m.stop))
  result.table <- rbind(result.table, result.h0)
  result.h1 <- table(factor(results[which(results[,2]==1),1], levels = conf_TASK_BMX2SEQTEST$lower_m:m.stop))
  result.table <- rbind(result.table, result.h1)
  result.no <- table(factor(results[which(results[,2]==2),1], levels = conf_TASK_BMX2SEQTEST$lower_m:m.stop))
  result.table <- rbind(result.table, result.no)
  props <- t(t(rowSums(result.table)/sim_count))
  colnames(props) <- c("prop")
  result.table <- cbind(result.table, props)
  row.names(result.table) <- c("accept H0:", "accept H1:", "other:")
  # 计算错误率
  m.fixed <- computeExpectStopTime(c(result.h0), c(result.h1), c(result.no), sim_count)
  conf_TASK_BMX2SEQTEST <- list(
    est_type = est.type,
    alpha = alpha,
    beta  = beta,
    agree_cnt = agr.cnt,
    relative = relative,
    var.est.conf = list(
      name = "var_est3_mx2cv"
    ),
    delta_0 = delta0,
    delta_1 = delta1,
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
  m.stop  <- upper_m + agr.cnt-1
  result.table.mfix <- c()
  result.h0.mfix <- table(factor(results[which(results[,2]==0),1], levels = conf_TASK_BMX2SEQTEST$lower_m:m.stop))
  result.table.mfix <- rbind(result.table.mfix, result.h0.mfix)
  result.h1.mfix <- table(factor(results[which(results[,2]==1),1], levels = conf_TASK_BMX2SEQTEST$lower_m:m.stop))
  result.table.mfix <- rbind(result.table.mfix, result.h1.mfix)
  result.no.mfix <- table(factor(results[which(results[,2]==2),1], levels = conf_TASK_BMX2SEQTEST$lower_m:m.stop))
  result.table.mfix <- rbind(result.table.mfix, result.no.mfix)
  props.mfix <- t(t(rowSums(result.table.mfix)/sim_count))
  type1error <- mean(results[,3])
  type2error <- mean(results[,4])
  
  format.result <- c(result.h0, result.h1, result.no, m.fixed, c(props), c(props.mfix), type1error, type2error)
  colnames(format.result) <- NULL
  names(format.result) <- NULL
  return(format.result)
}

testWithExternFileParallel_bmx2 <- function(delta.start, delta.end, delta.step, true.value, file.name, upper.m, est.type, dataset.tag, delta.equal=T, rho1=NULL, rho2=NULL) {
  library(foreach)
  library(doParallel)
  source("./tests/testsCollectDatasets.R", encoding="UTF-8")
  work.directory <- getwd()
  delta.seq <- seq(delta.start, delta.end, delta.step)
  delta.seq <- c(delta.seq, true.value)
  delta.seq <- sort(delta.seq)
  sim.count <- NULL
  if(delta.equal == F)
    sim.count <- length(delta.seq) * (length(delta.seq)+1)/2
  else 
    sim.count <- length(delta.seq)
  m.expect <- NULL
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  results <- foreach(i=1:sim.count, .combine = rbind, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    if(delta.equal == F) {
      sum <- 0
      k <- 0
      for(j in 0:i) {
        sum =sum+j
        if(sum >= i) {
          sum <- sum-j
          j <- j-1
          break
        }
        k <- k+1
      }
      delta1.index <-  k 
      delta0.index <- i-sum
      delta1 <- delta.seq[delta1.index]
      delta0 <- delta.seq[delta0.index]
    } else {
      delta1 <- delta.seq[i]
      delta0 <- delta.seq[i]
    }
    result <- NULL
    result <- testMx2BCVSeqTest_bmx2cv(upper.m, delta0, delta1,  file.name=file.name, est.type=est.type, rho1=rho1, rho2=rho2)
    c(delta0, delta1, result)
  }
  write.csv(results, file = paste("est_for_test/", paste("bmx2", dataset.tag, est.type, format(Sys.time(), "%Y%m%d%H%M%S"),sep="_"), sep=""))
}

testWithExternFileParallel_diet <- function(delta.start, delta.end, delta.step, true.value, file.name, upper.m, dataset.tag, delta.equal=T) {
  library(foreach)
  library(doParallel)
  source("./tests/testsCollectDatasets.R", encoding="UTF-8")
  work.directory <- getwd()
  delta.seq <- seq(delta.start, delta.end, delta.step)
  delta.seq <- c(delta.seq, true.value)
  delta.seq <- sort(delta.seq)
  sim.count <- NULL
  if(delta.equal == F)
    sim.count <- length(delta.seq) * (length(delta.seq)+1)/2
  else 
    sim.count <- length(delta.seq)
  cl <- makeCluster(type="MPI")
  registerDoParallel(cl)
  m.expect <- NULL
  results <- foreach(i=1:sim.count, .combine = rbind, .export = ls(.GlobalEnv)) %dopar% {
    setwd(work.directory)
    # 根据i换算出delta值
    if(delta.equal == F) {
      sum <- 0
      k <- 0
      for(j in 0:i) {
        sum =sum+j
        if(sum >= i) {
          sum <- sum-j
          j <- j-1
          break
        }
        k <- k+1
      }
      delta1.index <-  k 
      delta0.index <- i-sum
      delta1 <- delta.seq[delta1.index]
      delta0 <- delta.seq[delta0.index]
    } else {
      delta1 <- delta.seq[i]
      delta0 <- delta.seq[i]
    }
    result <- NULL
    result <- testMx2BCVSeqTest_dietterich(upper.m, delta0, delta1, file.name=file.name) 
    c(delta0, delta1, result)
  }
  write.csv(results, file = paste("est_for_test/", paste("diet", dataset.tag, format(Sys.time(), "%Y%m%d%H%M%S"),sep="_"), sep=""))
}