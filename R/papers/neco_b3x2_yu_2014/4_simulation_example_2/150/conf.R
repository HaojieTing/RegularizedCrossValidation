TASK_COV_SIMU <- "covariance function simulation"
TASK_COV_PARM <- "covariance parameter simulation"
TASK_VAR_SIM  <- "variance simulation"
TASK_VAR_EST  <- "variance estimation"
TASK_TRUE_CI  <- "true confidential interval simulation"
TASK_SIGN_TEST <- "signification test for models"

task <- TASK_VAR_SIM

n <- 150

dataConf <- list(name = "two_normals_classification_y_random", 
                 type = "classification", 
                 n = n, 
                 y_p = 0.5, 
                 mu0 = rep(0, 5),
                 mu1 = rep(1, 5),
                 sigma0 = diag(5),
                 sigma1 = 2 * diag(5)
)

cvConf <- list(name = "balancedmx2cv", 
               m = 3, 
               n = n)


algorConf <- list(name = "logisticGLM",
                  type = "classification")



conf_TASK_VAR_SIM <- list(data_rep_count = 100000, 
                          split_rep_count = 1, 
                          var_for_index = 0,
                          seq_mode = FALSE,
                          cov_print = TRUE
)