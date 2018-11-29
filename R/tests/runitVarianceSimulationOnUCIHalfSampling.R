
testVarianceSimulationOnUCIDataSet_RHS <- function(uci.dataname, n, J, algor.name, data.rep=1000, part.rep=100, dataset.seed = 0, partitionset.seed= 1111111) {
  algorithm.confs <- list(
    "knn" = list(
      type = "classification",
      name = "knn",
      kernal = "triangular",
      k = 5
    ),
    "svm" = list(
      type = "classification",
      name = "svm"
    )
  )
  n1 <- round(n/2)
  source("./tasks/variance_simulation_cv.R", encoding = "UTF-8")
  conf_TASK_Bmx2CV_Var <- list(
    data_rep_cnt = data.rep,
    part_set_rep_cnt = part.rep,
    rnd_seed = FALSE,
    seq = F,
    dataset.conf = list(
      type =  "classification",
      name =  uci.dataname,
      samplingConf = list(
        n = n
      )
    ),
    crossvalidation.conf = list(
      name =  "rlt",
      n1 = n1,
      J = J
    ),
    algorithm.confs = list(
      "algorA" = algorithm.confs[[algor.name]]
    ),
    dataset.seed = dataset.seed,
    partitionset.seed = partitionset.seed
  )
  task_config <- variance_simulation_cv.task_config_validation(conf_TASK_Bmx2CV_Var)
  result      <- variance_simulation_cv.perform_task(task_config)
  save(result, file = paste("rhs_var_sim", uci.dataname, n, n1, J, algor.name, data.rep, part.rep, sep = "_"))
  write.csv(result[[2]], file = paste(paste("rhs_var_sim", uci.dataname, n, n1, J, algor.name, data.rep, part.rep, format(Sys.time(), "%Y%m%d%H%M%S"), sep = "_"), "csv", sep="."))
  print(result[[2]])
}


testVarianceSimulationOnUCIDataSet_rmx2cv <- function(uci.dataname, n, m, algor.name, data.rep=1000, part.rep=100, dataset.seed = 0, partitionset.seed= 1111111) {
  algorithm.confs <- list(
    "knn" = list(
      type = "classification",
      name = "knn",
      kernal = "triangular",
      k = 5
    ),
    "svm" = list(
      type = "classification",
      name = "svm"
    )
  )
  source("./tasks/variance_simulation_cv.R", encoding = "UTF-8")
  conf_TASK_Bmx2CV_Var <- list(
    data_rep_cnt = data.rep,
    part_set_rep_cnt = part.rep,
    rnd_seed = FALSE,
    seq = F,
    dataset.conf = list(
      type =  "classification",
      name =  uci.dataname,
      samplingConf = list(
        n = n
      )
    ),
    crossvalidation.conf = list(
      name =  "mxkrcv",
      v = 2,
      m = m
    ),
    algorithm.confs = list(
      "algorA" = algorithm.confs[[algor.name]]
    ),
    dataset.seed = dataset.seed,
    partitionset.seed = partitionset.seed
  )
  task_config <- variance_simulation_cv.task_config_validation(conf_TASK_Bmx2CV_Var)
  result      <- variance_simulation_cv.perform_task(task_config)
  save(result, file = paste("rmx2cv_var_sim_", uci.dataname, n, m, algor.name, data.rep, part.rep, sep = "_"))
  write.csv(result[[2]], file = paste(paste("rmx2cv_var_sim_", uci.dataname, n, m, algor.name, data.rep, part.rep, format(Sys.time(), "%Y%m%d%H%M%S"), sep = "_"), "csv", sep="."))
  print(result[[2]])
}


testVarianceSimulationOnUCIDataSet_bmx2cv <- function(uci.dataname, n, m, algor.name, data.rep=1000, part.rep=100, dataset.seed = 0, partitionset.seed= 1111111) {
  algorithm.confs <- list(
    "knn" = list(
      type = "classification",
      name = "knn",
      kernal = "triangular",
      k = 5
    ),
    "svm" = list(
      type = "classification",
      name = "svm"
    )
  )
  source("./tasks/variance_simulation_cv.R", encoding = "UTF-8")
  conf_TASK_Bmx2CV_Var <- list(
    data_rep_cnt = data.rep,
    part_set_rep_cnt = part.rep,
    rnd_seed = FALSE,
    seq = F,
    dataset.conf = list(
      type =  "classification",
      name =  uci.dataname,
      samplingConf = list(
        n = n
      )
    ),
    crossvalidation.conf = list(
      name =  "mx2bcv",
      m = m
    ),
    algorithm.confs = list(
      "algorA" = algorithm.confs[[algor.name]]
    ),
    dataset.seed = dataset.seed,
    partitionset.seed = partitionset.seed
  )
  task_config <- variance_simulation_cv.task_config_validation(conf_TASK_Bmx2CV_Var)
  result      <- variance_simulation_cv.perform_task(task_config)
  save(result, file = paste("bmx2cv_var_sim_", uci.dataname, n, m, algor.name, data.rep, part.rep, sep = "_"))
  write.csv(result[[2]], file = paste(paste("Bmx2cv_var_sim_", uci.dataname, n, m, algor.name, data.rep, part.rep, format(Sys.time(), "%Y%m%d%H%M%S"), sep = "_"), "csv", sep="."))
  print(result[[2]])
}

testVarianceSimulationOnUCIDataSet_BRHS <- function(uci.dataname, n, J, algor.name, data.rep=1000, part.rep=100, dataset.seed = 0, partitionset.seed= 1111111) {
  algorithm.confs <- list(
    "knn" = list(
      type = "classification",
      name = "knn",
      kernal = "triangular",
      k = 5
    ),
    "svm" = list(
      type = "classification",
      name = "svm"
    )
  )
  n1 <- round(n/2)
  source("./tasks/variance_simulation_cv.R", encoding = "UTF-8")
  conf_TASK_Bmx2CV_Var <- list(
    data_rep_cnt = data.rep,
    part_set_rep_cnt = part.rep,
    rnd_seed = FALSE,
    seq = F,
    dataset.conf = list(
      type =  "classification",
      name =  uci.dataname,
      samplingConf = list(
        n = n
      )
    ),
    crossvalidation.conf = list(
      name =  "rhsbcv",
      n1 = n1,
      J = J
    ),
    algorithm.confs = list(
      "algorA" = algorithm.confs[[algor.name]]
    ),
    dataset.seed = dataset.seed,
    partitionset.seed = partitionset.seed
  )
  task_config <- variance_simulation_cv.task_config_validation(conf_TASK_Bmx2CV_Var)
  result      <- variance_simulation_cv.perform_task(task_config)
  save(result, file = paste("brhs_var_sim", uci.dataname, n, n1, J, algor.name, data.rep, part.rep, sep = "_"))
  write.csv(result[[2]], file = paste(paste("brhs_var_sim", uci.dataname, n, n1, J, algor.name, data.rep, part.rep, format(Sys.time(), "%Y%m%d%H%M%S"), sep = "_"), "csv", sep="."))
  print(result[[2]])
}