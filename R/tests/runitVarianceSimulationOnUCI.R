
testVarianceSimulationOnUCIDataSet_RLT <- function(uci.dataname, n, n1, J, algor.name, data.rep=5000, part.rep=200) {
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
      name =  "rlt",
      n1 = n1,
      J = J
    ),
    algorithm.confs = list(
      "algorA" = algorithm.confs[[algor.name]]
    )
  )
  task_config <- variance_simulation_cv.task_config_validation(conf_TASK_Bmx2CV_Var)
  result      <- variance_simulation_cv.perform_task(task_config)
  save(result, file = paste("rlt_var_sim", uci.dataname, n, n1, J, algor.name, data.rep, part.rep, sep = "_"))
  write.csv(result[[2]], file = paste(paste("rlt_var_sim", uci.dataname, n, n1, J, algor.name, data.rep, part.rep, sep = "_"), "csv", sep="."))
  print(result[[2]])
}

testVarianceSimulationOnUCIDataSet_KFCV <- function(uci.dataname, n, v, algor.name, data.rep=5000, part.rep=200) {
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
      name =  "standardVFCV",
      v = v
    ),
    algorithm.confs = list(
      "algorA" = algorithm.confs[[algor.name]]
    )
  )
  task_config <- variance_simulation_cv.task_config_validation(conf_TASK_Bmx2CV_Var)
  result      <- variance_simulation_cv.perform_task(task_config)
  save(result, file = paste("kfcv_var_sim_", uci.dataname, n, v, algor.name, data.rep, part.rep, sep = "_"))
  write.csv(result[[2]], file = paste(paste("kfcv_var_sim",uci.dataname, n, v, algor.name, data.rep, part.rep, sep = "_"), "csv", sep="."))
  print(result[[2]])
}