source("../datasets/data_loader.R", encoding="UTF-8")
source("../mlAlgorithms/algorLoader.R", encoding="UTF-8")
source("../crossvalidations/cv_loader.R", encoding="UTF-8")
source("../modelTrainAndPredict.R", encoding="UTF-8")
source("../utils.R", encoding="UTF-8")

alpha <- conf_TASK_BMX2INCTEST$alpha         
veConf <- conf_TASK_BMX2INCTEST$var_est_conf 
seq_mode <- conf_TASK_BMX2INCTEST$seq_mode   
rpt <-  conf_TASK_BMX2INCTEST$data_rep_count 
split_group_number <- conf_TASK_BMX2INCTEST$split_rep_count 
setPartitionSeed <- conf_TASK_BMX2INCTEST$splitSeed  
veConf2 <- conf_TASK_BMX2INCTEST$var_est_conf2  
mu_diff <- conf_TASK_BMX2INCTEST$mudiff
beta  <- conf_TASK_BMX2INCTEST$beta
m_upper_h0 <- conf_TASK_BMX2INCTEST$Mh0         
threshold_h1 <- conf_TASK_BMX2INCTEST$threshold     


total_repetition = rpt * split_group_number
DataInfo <- loadDataGenerator(dataConf)
DataGenerator <- DataInfo[[1]]
DataPackages <- DataInfo[[2]]
AlgorInfo <- mlalgorLoader(algorConf)
AlgorGenerator <- AlgorInfo[[1]]
AlgorPackages <- AlgorInfo[[2]]

AlgorInfo2 <- mlalgorLoader(algorConf2)
AlgorGenerator2 <- AlgorInfo2[[1]]
AlgorPackages2 <- AlgorInfo2[[2]]

if(cvConf$name != 'increaseBalancedMx2cv') {
  stop("type of cross validation needed：increaseBalancedMx2cv");
}

CrossValidationInfo <- loadCrossValidationGenerator(cvname = cvConf$name)
CrossValidationGenerator <- CrossValidationInfo[[1]]
CrossValidationPackages <- CrossValidationInfo[[2]]

workdirpath <- getwd()

if(FALSE == seq_mode) {
  library(snow)
  
  parfun <- function(rpt_number) {
    setwd(workdirpath)
    dataIndex <- ((rpt_number-1) %% rpt) + 1 
    group <- (rpt_number-1) %/% rpt          
    set.seed(dataIndex) 
    data <- DataGenerator(dataConf)
    n <- nrow(data)
    stat <- n    
    if(setPartitionSeed){
      set.seed(group+1111111)
    }    
    cvConf$data <- data  
    muv1 <- c()
    muv2 <- c()
    prev_stat <- FALSE
    stat <- FALSE
    consis_count <- 0
    p_value <- NA
    while(TRUE) {
      cvConf <- CrossValidationGenerator(cvConf)
      m <- cvConf$m
      split <- cvConf$splits_new
      cvres1 <- genericTrainAndTestForMultiSplits(data, split, AlgorGenerator, algorConf)
      cvres2 <- genericTrainAndTestForMultiSplits(data, split, AlgorGenerator2, algorConf2)      
      muv1 <- c(muv1, cvres1[[2]])
      muv2 <- c(muv2, cvres2[[2]])      
      muvdiff <- muv1 - muv2
      ve.estimator <- loadVarEstForOneExprInfo(vename = veConf$name)
      varest <- ve.estimator(c(muvdiff, mean(muvdiff)), cvConf$m, veConf)
      diff_mean <- abs(mean(muvdiff))
      Tstat <- diff_mean /sqrt(varest)
      ve.freedegree <- loadFreeDegreeWrtVE(vename =veConf$name)
      freedegree <-  ve.freedegree(cvConf$m)
      t_value <- qt(1 - alpha/2, freedegree)
      stat <- Tstat > t_value 
      p_value <- 2*pt(-abs(Tstat),df = freedegree)
      if(stat) {
        if(prev_stat) {
           consis_count <- consis_count + 1
        } else {
           consis_count <- 1
        }
        prev_stat <- TRUE
      } else {
        prev_stat <- FALSE
        consis_count <- 0
      }
      if(consis_count > threshold_h1) {
         break
      }
      if(!is.null(m_upper_h0)) {
        if(cvConf$m > m_upper_h0) {
           break
        }
      }
      cvConf$m <- cvConf$m + 1
    }
    ano_stat <- NA
    ano_pvalue <- NA
    if(stat) {
      if(is.null(mu_diff)) {
        mu_diff <- abs(mean(muv1) - mean(muv2))
      }
      muvdiff <- muv1 - muv2
      ve.estimator2 <- loadVarEstForOneExprInfo(vename = veConf2$name)
      varest2 <- ve.estimator2(c(muvdiff, mean(muvdiff)), cvConf$m, veConf2)
      diff_mean <- abs(mean(muvdiff))
      Tstat <- abs(diff_mean - mu_diff) /sqrt(varest)
      ve.freedegree <- loadFreeDegreeWrtVE(vename =veConf$name)
      freedegree <-  ve.freedegree(cvConf$m)
      t_value <- qt(1 - beta/2, freedegree)
      ano_stat <- Tstat > t_value 
      ano_pvalue <- 2*pt(-abs(Tstat),df = freedegree)
    }
    list(stat, cvConf$m, p_value, ano_stat, ano_pvalue)
  }
  
  cl <- makeCluster() 
  ignore <- clusterCall(cl, worker.init, c("tools"))
  ignore <- clusterCall(cl, worker.init, DataPackages)
  ignore <- clusterCall(cl, worker.init, AlgorPackages)
  ignore <- clusterCall(cl, worker.init, CrossValidationPackages)
  ignore <- clusterCall(cl, worker.init, AlgorPackages2)
  allfunsAndVariables <- ls()
  ignore<-clusterExport(cl, allfunsAndVariables)
  result <- clusterApplyLB(cl, 1:total_repetition, parfun)
  stopCluster(cl)  
  stats <- do.call(rbind, lapply(result, "[[", 1))
  m_all <-  do.call(rbind, lapply(result, "[[", 2))
  pvalues <-  do.call(rbind, lapply(result, "[[", 3))
  ano_stats <-  do.call(rbind, lapply(result, "[[", 4))
  ano_pvalues <-  do.call(rbind, lapply(result, "[[", 5))
  typeIErrorH0 <- mean(stats)
  typeIErrorH1 <- NA
  countH1 <- NA
  countH1Acpt <- NA
  if(length(which(!is.na(ano_stats))) > 0){
    ano_stats_dec <- ano_stats[which(!is.na(ano_stats))]
    countH1 <- length(ano_stats_dec)
    countH1Acpt <- length(ano_stats_dec)
    typeIErrorH1 <- mean(ano_stats_dec)
  }
} else 
{
  stop("not be implemented.")
}
mavg_reject <- mean(m_all[which(stats == TRUE)])
mavg_accept <- mean(m_all[which(stats == FALSE)])
write.table(mavg_reject, file= paste(data_dir, "mrej.csv", sep=.Platform$file.sep))
write.table(mavg_accept, file= paste(data_dir, "macpt.csv", sep=.Platform$file.sep))
write.table(m_all, file= paste(data_dir, "mall.csv", sep=.Platform$file.sep))
write.table(stats, file = paste(data_dir, "test_result_h0.csv", sep=.Platform$file.sep) )
write.table(typeIErrorH0, file = paste(data_dir, "error_prob_h0.csv", sep=.Platform$file.sep))
write.table(ano_stats, file = paste(data_dir, "test_result_h1.csv", sep=.Platform$file.sep) )
write.table(typeIErrorH1, file = paste(data_dir, "error_prob_h1.csv", sep=.Platform$file.sep))
write.table(countH1, file = paste(data_dir, "count_h1.csv", sep=.Platform$file.sep))
write.table(countH1Acpt, file = paste(data_dir, "count_h1_acpt.csv", sep=.Platform$file.sep))
write.table(pvalues, file = paste(data_dir, "p_values_h0.csv", sep=.Platform$file.sep))
write.table(ano_pvalues, file = paste(data_dir, "p_values_h1.csv", sep=.Platform$file.sep))