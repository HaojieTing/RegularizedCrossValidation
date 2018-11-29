

testBmx2CV_VarRegTest <- function() {
  source("./tasks/variance_simulation_cv.R", encoding = "UTF-8")
  conf_TASK_Bmx2CV_VarRegTest <- list(
    data_rep_cnt = 100,
    part_set_rep_cnt = 1,
    rnd_seed = FALSE,
    seq = T,
    dataset.conf = list(
      type =  "classification",
      name =  "two_normals_classification",
      shortcut_name =  "bengio_infer_ml_sim1",
      mu0 = rep(0,5),
      mu1 = rep(1,5),
      sigma0 = diag(5),
      sigma1 = 2*diag(5) 
    ),
    crossvalidation.conf = list(
      name =  "mx2bcv_inc",
      m =  3 
    ),
    algorithm.confs = list(
      "logistic" = list(  type =  "classification",  name =  "logisticGLM"),
      "ctree" = list(type="classification", name="classificationTree", method="class")
    )
  )
  
  task_config <- variance_simulation_cv.task_config_validation(conf_TASK_Bmx2CV_VarRegTest)
  result      <- variance_simulation_cv.perform_task(task_config)
  reg.result  <- variance_simulation_cv.compute_regularized_variance_estimator(result[[1]], task_config, extra_config = list(type="var_est", var.est.conf=list( name = "var_est3_mx2cv", m= 3)))
  summary.result <- variance_simulation_cv.summary_reg_result(reg.result, task_config, extra_config = list(is.min=T))
  print(summary.result)
}
