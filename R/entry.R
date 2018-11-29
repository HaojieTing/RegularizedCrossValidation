# 工具包入口文件
#
# 本工具包可用于执行如下任务:
# 1. 协方差函数图像的模拟。
# 2. 协方差函数中相关参数的模拟。
# 3. 各种交叉验证估计的偏差、方差模拟。
# 4. 各种交叉验证估计的方差的估计量计算。
# 5. 基于交叉验证估计的模型比较的显著性检验方法。
# 6. 基于mx2交叉验证估计的模型比较的序贯检验方法。
# 7. 泛化误差的真实置信区间的模拟算法。
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/5/9
# 
# TODO(wangruibo@2017/5/9): 除目前的0-1损失和平方损失外，加入对其它评价指标的支持。
#                           特别是，分类问题上，基于混淆矩阵的评价指标。
# TODO(wangruibo@2017/5/9): 对所有代码进行逐步规范,加入详细的注释,具体的规范，参照
#                           google的R语言编程规范:https://google.github.io/styleguide/Rguide.xml
# TODO(wangruibo@2017/5/9): 加入Redis服务器来存储实验配置和存储实验结果，目前，不做
#                           实现。参见: http://cos.name/2013/04/nosql-r-redis/以及
#                           https://cran.r-project.org/web/packages/rredis/

library(tools)

# 工具包中可实施任务定义列表
taskCovFunctionSimulation <- "covariance function simulation"  # 协方差函数模拟
taskVarianceSimulation  <- "variance simulation"  # 方差模拟
taskVarianceEstimation  <- "variance estimation"  # 方差估计
taskSignTest <- "signification test for models"  # 显著性检验
taskBmx2SeqTest <- "Balanced mx2 cross validation sequential t-test"  # 均衡mx2交叉验证的序列t-检验
taskBmx2IncTest <- "Balanced mx2 cross validation incremental t-test"  # 均衡mx2交叉验证的增量式t-检验
TASK_BMX2VARSIM <- "Balanced mx2 cross validation variance estimation"  # 均衡mx2交叉验证的方差估计
taskSignTestReplicability <- "replicability for sign test"  # 显著性检验的可重复度
taskTrueConfIntSimulation   <- "TRUE Confidence Interval Simulation"  # 真实置信区间模拟

config.file <-  NULL #任务的配置文件

if(.Platform$OS.type == "unix") {
  library("optparse")
  option_list <- list(  
    make_option(c("-f","--file"), default="", help="configuration file for experiments")
  )
  parser <- OptionParser(option_list=option_list)
  opt <- parse_args(parser)
  config.file <- opt$file
} else {
  args <- commandArgs()
  config.file <- args[grep("conf.R", args)]
  if (length(config.file) != 1) {
	  stop("Cannot Find the proper configuration script file: conf.R")
  }
}

#检查配置文件的扩展名是否为.R
ext <- file_ext(config.file)
if( "R" != ext) {
  stop("The configuration file of a task must with extension .R!")
}

data.dir <- dirname(config.file)
filenames <- list.files(data.dir)
if(1 != length(filenames)) {
    stop("Parent directory of the configuration file cannot contain other files or directories!")
}


# 引入实验配置文件
source(config.file, encoding = "UTF-8")

if (!exists("task.conf")) { #如果不存在任务配置
  stop("need to specify task configurations!")
}
task.type <- task.conf$type #取出任务类型
if (is.null(task.type)) { #如果任务不存在类型.
  stop("Please specify task type!")
}

# 协方差函数模拟模块调用
if (task.type == taskCovFunctionSimulation) {
  ValidateAndResolveDataSetConfiguration()
  ValidateAndResolveAlgorithmConfiguration()
  ValidateAndResolveCrossValidationConfiguration()
  setwd("./covfunSimulation/")
  source("config.R", encoding = "UTF-8")
}

# 方差模拟模块调用
if (task.type == taskVarianceSimulation) {
  ValidateAndResolveDataSetConfiguration()
  ValidateAndResolveAlgorithmConfiguration()
  ValidateAndResolveCrossValidationConfiguration()
  setwd("./varianceSimulation/")
  source("config.R", encoding = "UTF-8")
}

# 方差估计模块调用
if (task.type == taskVarianceEstimation) {
  if( !exists("var.estimator.conf") ) {
      stop("Need to specify the configuration of variance estimation TASK[TASK_VAR_EST]")
    }
    source('./varianceEstimator/var_est_loader.R', encoding="UTF-8")
    validate_status <- validateVarEstConf(var.estimator.conf)
    if( !validate_status ) {
      stop("configuration of variance estimation is incorrect.[VE]")
    }
    ve.estimator <- loadVarEstInfo(vename = var.estimator.conf$name)
    result <- ve.estimator(var.estimator.conf)
    if(is.null(var.estimator.conf$multi) || var.estimator.conf$multi == FALSE){
      save(result, file = file.path(data_dir, "data"))
      fileconn <- file(paste(data_dir, "result.txt", sep=.Platform$file.sep))
      result_mean <- mean(result)
      result_var <- var(result)
      writeLines(c("Mean:\n", result_mean, "\nVar\n", result_var ), fileconn)
      close(fileconn)
    } else {
      save(result, file = file.path(data_dir, "data"))
      output <- colMeans(result)
      write.csv(output, file=paste(data_dir, "multioutput.csv", sep=.Platform$file.sep))
    }
}

# 算法真实性能值模拟
if (task.type == taskTrueConfIntSimulation) {
  source("./tasks/true_performance_simulation/config.R")
}

# 显著性检验模块
if (task.type == taskSignTest) {
    source('./varianceEstimator/var_est_loader.R', encoding="UTF-8")
    source("./signif_Test/sign_test_loader.R", encoding= "UTF-8")
    validate.status <- validateSignTestInfo(testing.conf)
    if( !validate_status )  {
      stop("Configuration of significant test is incorrect.[ST]")
    }
    st.processor <- loadSignTestFunction(testing.conf$test_type)
    result <- st.processor(testing.conf)
    write.table(result$typeI_error, file = paste(data_dir, "error_prob.csv", sep=.Platform$file.sep))
    write.table(result$z, file = paste(data_dir, "z.csv", sep=.Platform$file.sep) )
    write.table(result$potential, file = paste(data_dir, "potential.csv", sep=.Platform$file.sep) )
    write.table(result$test_result, file = paste(data_dir, "test_result.csv", sep=.Platform$file.sep) )
    if(!is.null(result$p_values)){
      write.table(result$p_values, file=paste(data_dir, "p_values.csv", sep=.Platform$file.sep))
    }
}


# if( taskSignTest == task.type || taskVarianceEstimation == task.type 
#     ||  taskSignTestReplicability == task.type || taskTrueConfIntSimulation == task.type) {
#   # 当要执行的任务为上述的四种任务时，不需要对配置文件进行校验
#   need.validate <- FALSE
# }
# 
# 
# 
# #取出task的类型.
# task.type <- task.conf$type
# if ( taskVarianceSimulation == task.type  ) {
#     if ( is.null(task.conf$data_rep_count) 
#                 || is.null(task.conf$split_rep_count) 
#                 || is.null(task.conf$seq_mode)) {
#       stop("Need to specify the repitition counts for datasets and splits in task configuration!")
#     }  
#     setwd("./varianceSimulation/")
#     source("config.R", encoding = "UTF-8")
# } else
# 
# if ( taskSignTest == task.type) {
#   source('./varianceEstimator/var_est_loader.R', encoding="UTF-8")
#   source("./signif_Test/sign_test_loader.R", encoding= "UTF-8")
#   validate.status <- validateSignTestInfo(conf_TASK_SIGN_TEST)
#   if( !validate_status )  {
#     stop("Configuration of significant test is incorrect.[ST]")
#   }
#   st.processor <- loadSignTestFunction(conf_TASK_SIGN_TEST$test_type)
#   result <- st.processor(conf_TASK_SIGN_TEST)
#   write.table(result$typeI_error, file = paste(data_dir, "error_prob.csv", sep=.Platform$file.sep))
#   write.table(result$z, file = paste(data_dir, "z.csv", sep=.Platform$file.sep) )
#   write.table(result$potential, file = paste(data_dir, "potential.csv", sep=.Platform$file.sep) )
#   write.table(result$test_result, file = paste(data_dir, "test_result.csv", sep=.Platform$file.sep) )
#   if(!is.null(result$p_values)){
#     write.table(result$p_values, file=paste(data_dir, "p_values.csv", sep=.Platform$file.sep)) 
#   }    
# } else if ( TASK_VAR_EST == task ) { 
#   if( !exists("conf_TASK_VAR_EST") ) {
#     stop("Need to specify the configuration of variance estimation TASK[TASK_VAR_EST]")
#   }
#   source('./varianceEstimator/var_est_loader.R', encoding="UTF-8")
#   validate_status <- validateVarEstConf(conf_TASK_VAR_EST)
#   if( !validate_status ) {
#     stop("configuration of variance estimation is incorrect.[VE]")
#   }
#   ve.estimator <- loadVarEstInfo(vename = conf_TASK_VAR_EST$name)
#   result <- ve.estimator(conf_TASK_VAR_EST)
#   if(is.null(conf_TASK_VAR_EST$multi) || conf_TASK_VAR_EST$multi == FALSE){ 
#     
#     save(result, file = file.path(data_dir, "data"))
#     
#     fileconn <- file(paste(data_dir, "output.txt", sep=.Platform$file.sep))
#     result_mean <- mean(result)
#     result_var <- var(result)
#     writeLines(c("Mean:\n", result_mean, "\nVar\n", result_var ), fileconn)
#     close(fileconn)
#   } else {
#     save(result, file = file.path(data_dir, "data"))
#     output <- colMeans(result)
#     write.csv(output, file=paste(data_dir, "multioutput.csv", sep=.Platform$file.sep))
#   }
#   
# } else if ( TASK_COV_SIMU == task || TASK_COV_PARM == task) { 
#   if( !exists("conf_TASK_COV_SIMU") ) {
#     stop("Need to specify the configuration of covariance function[TASK_COV_SIMU]")
#   } 
#   setwd("./covfunSimulation/")
#   source("config.R", encoding = "UTF-8")
# } else if( TASK_REPLICABILITY == task) {
#   if( !exists("conf_TASK_REPLICABILITY") ) {
#     stop("Need to specify the configuration of replicability task")  
#   } else{ if(is.null(conf_TASK_REPLICABILITY$test_count) || 
#               is.null(conf_TASK_REPLICABILITY$typeI_count_file_names))
#     warning("")
#   }
#   source('./replicability_sign_test/replicability_util.R', encoding = "UTF-8")
#   test_count <- conf_TASK_REPLICABILITY$test_count
#   file_names <- conf_TASK_REPLICABILITY$typeI_count_file_names
#   type1_total_counts <- rep(NA, length(file_names))
#   type1_counts <- rep(NA, length(file_names))
#   index <- 1
#   for( filename in file_names) {
#     fexist <- file.exists(filename)
#     if( FALSE == fexist ) {
#       stop( paste("FILE NOT FOUND:", filename) )
#     }
#     load(filename)
#     type1_counts[index] <- sum(as.numeric(test_result))
#     type1_total_counts <- length(test_result)
#     rm(test_result)  
#   }
#   
#   for(i in 1:length(type1_total_counts)) {
#      total_count <- type1_total_counts[i]
#      if(total_count != test_count)
#        stop(paste("total counts of tests are not correct:", type1_total_counts))
#   }
#   result <- Replicability.compute(typeI_counts = type1_counts, test_count = test_count)
#   fileconn <- file(paste(data_dir, "output.txt", sep=.Platform$file.sep))  
#   writeLines(c("consistent count:", result[[1]], "\nalmost consistent count:", result[[2]], 
#                "\nreplicability:", result[[3]]), fileconn)
#   close(fileconn)
# } else 
# 
# 
# 
# if( TASK_BMX2SEQTEST == task ) {
#   
#   if( !exists("algorConf2") ) {
#     stop("need to specify the second algorithm configuration")
#   }
#   
#   
#   validation_status <- validateAlgorInfo(algorConf2) 
#   if(validation_status == FALSE ) {
#     stop("Failed to validate the second algorithm configuration[ALGOR]")
#   }
#   algorConf2 <- parseAlgorInfoShortcut(algorConf2)
#   if( !exists("conf_TASK_BMX2SEQTEST")) {
#     stop("Need to specify the configuration of TASK_BMX2SEQTEST TASK")
#   }
#   source('./varianceEstimator/var_est_loader.R', encoding="UTF-8")
#   setwd("./bmx2seqtest/")
#   source("config.R", encoding = "UTF-8")
# } else
# 
# if( TASK_BMX2INCTEST == task ) {
#   
#   if( !exists("algorConf2") ) {
#     stop("need to specify the second algorithm configuration")
#   }
#   
#   
#   validation_status <- validateAlgorInfo(algorConf2) 
#   if(validation_status == FALSE ) {
#     stop("Failed to validate the second algorithm configuration[ALGOR]")
#   }
#   algorConf2 <- parseAlgorInfoShortcut(algorConf2)
#   if( !exists("conf_TASK_BMX2INCTEST")) {
#     stop("Need to specify the configuration of TASK_BMX2INCTEST TASK")
#   }
#   source('./varianceEstimator/var_est_loader.R', encoding="UTF-8")
#   setwd("./bmx2inctest/")
#   source("config.R", encoding = "UTF-8")
# } else
#   
# if( TASK_BMX2VARSIM == task) {
#   if( !exists("conf_TASK_VAR_SIM") ) {
#     stop("Need to specify the configuration for variance simulation task[TAKS_VAR_SIM]")
#   } else if ( is.null(conf_TASK_VAR_SIM$data_rep_count) 
#               || is.null(conf_TASK_VAR_SIM$split_rep_count) 
#               || is.null(conf_TASK_VAR_SIM$seq_mode)) {
#     stop("Need to specify the repitition counts for datasets and splits[TAKS_VAR_SIM]")
#   }  
#   setwd("./varsim_incmx2cv/")
#   source("conf.R", encoding = "UTF-8")
# }