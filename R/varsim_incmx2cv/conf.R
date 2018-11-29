source("../datasets/data_loader.R", encoding="UTF-8")
source("../mlAlgorithms/algorLoader.R", encoding="UTF-8")
source("../crossvalidations/cv_loader.R", encoding="UTF-8")
source("../modelTrainAndPredict.R", encoding="UTF-8")
source("../utils.R", encoding="UTF-8")


rpt <- conf_TASK_VAR_SIM$data_rep_count
split_group_number <- conf_TASK_VAR_SIM$split_rep_count
seq_mode <- conf_TASK_VAR_SIM$seq_mode
var_for_index <- conf_TASK_VAR_SIM$var_for_index
cov_print <- TRUE
setPartitionSeed <- conf_TASK_VAR_SIM$splitSeed
if( is.null(setPartitionSeed) )
  setPartitionSeed <- TRUE
if( is.null(conf_TASK_VAR_SIM$cov_print) ) {
  cov_print <- FALSE
} else {
  cov_print <- conf_TASK_VAR_SIM$cov_print
}

total_repetition = rpt * split_group_number
DataInfo <- loadDataGenerator(dataConf)
DataGenerator <- DataInfo[[1]]
DataPackages <- DataInfo[[2]]

AlgorInfo <- mlalgorLoader(algorConf)
AlgorGenerator <- AlgorInfo[[1]]
AlgorPackages <- AlgorInfo[[2]]

if(cvConf$name != 'increaseBalancedMx2cv') {
   stop(" cv type need: increaseBalancedMx2cv");
}
CrossValidationInfo <- loadCrossValidationGenerator(cvname = cvConf$name)
CrossValidationGenerator <- CrossValidationInfo[[1]]
CrossValidationPackages <- CrossValidationInfo[[2]]

workdirpath <- getwd()
real_m <- cvConf$m
if(FALSE == seq_mode) {
  library(snow)  
  parfun<-function(rpt_number){
    setwd(workdirpath)
    dataIndex <- ((rpt_number-1) %% rpt) + 1 
    group <- (rpt_number-1) %/% rpt          
    
    set.seed(dataIndex) 
    data <- DataGenerator(dataConf)
    n <- nrow(data)    
    
    if(setPartitionSeed){
      set.seed(group+1111111)
    }
    cvConf$data <- data
    m <- cvConf$m
    muv <- c()
    for(cm in 2:m) {
      cvConf$m <- cm    
      cvConf <- CrossValidationGenerator(cvConf)
      split <- cvConf$splits_new
      cvres <- genericTrainAndTestForMultiSplits(data, split, AlgorGenerator, algorConf)      
      muv <- c(muv,cvres[[2]])
    }
    muv
  }    
  
  allfunsAndVariables <- ls()
  cl <- makeCluster() 
  
  ignore <- clusterCall(cl, worker.init, DataPackages)
  
  ignore <- clusterCall(cl, worker.init, AlgorPackages)
  
  ignore <- clusterCall(cl, worker.init, CrossValidationPackages)
  ignore<-clusterExport(cl, allfunsAndVariables)
  muv <- clusterApplyLB(cl, 1:total_repetition, parfun)
  stopCluster(cl)  
  muv <- do.call(rbind, muv)
} else {
  stop("sequential mode is not supported");
}



muv[is.na(muv) ] <- 0.0

if(ncol(muv) != 2* real_m) {
  stop("the column number of muv is wrong")
}

for(m in 2:real_m) {

  t_muv <- muv[,1:(2*m)]
  means_vec <- rowMeans(t_muv)
  if(length(means_vec) != total_repetition) {
    stop("mean vector length is wrong")
  }
  total_var <- NA
  total_expect <- NA
  if(split_group_number != 1 && rpt != 1) {

    mumat <- matrix(means_vec, split_group_number) 
    vars <- apply(mumat, 1, var)
    total_var <- mean(vars)
    total_expect <- mean(mumat)
  }else if(split_group_number == 1) {
    total_var <- var(mean_vec)
    total_expect <- mean(mean_vec)
  }else if(rpt == 1){
    total_var <- var(mean_vec)
    total_expect <- mean(mean_vec)
  }
  write.table(total_var, file=paste(data_dir, paste("var_", m, "csv",sep="."), sep=.Platform$file.sep))
  write.table(total_expect, file=paste(data_dir, paste("mean_", m, "csv",sep="."), sep=.Platform$file.sep))
}

save(muv, file=paste(data_dir, "data", sep=.Platform$file.sep))

