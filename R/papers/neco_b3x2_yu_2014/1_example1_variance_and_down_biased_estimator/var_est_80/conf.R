TASK_COV_SIMU <- "covariance function simulation"
TASK_COV_PARM <- "covariance parameter simulation"
TASK_VAR_SIM  <- "variance simulation"
TASK_VAR_EST  <- "variance estimation"
TASK_TRUE_CI  <- "true confidential interval simulation"
TASK_SIGN_TEST <- "signification test for models"

task <- TASK_VAR_EST

data_dir <- "/apps/users/wrb/trunk/papers/neco_yu/example1"

conf_TASK_VAR_EST <- list(
                    name = "var_est_dietterich_mx2cv",
                    data_file = file.path(data_dir, "var_40", "data", fsep=.Platform$file.sep),
                    m = 5
  )
