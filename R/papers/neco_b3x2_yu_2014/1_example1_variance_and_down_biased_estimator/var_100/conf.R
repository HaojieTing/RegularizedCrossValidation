TASK_COV_SIMU <- "covariance function simulation"
TASK_COV_PARM <- "covariance parameter simulation"
TASK_VAR_SIM  <- "variance simulation"
TASK_VAR_EST  <- "variance estimation"
TASK_TRUE_CI  <- "true confidential interval simulation"
TASK_SIGN_TEST <- "signification test for models"

task <- TASK_VAR_SIM

n <- 100

dataConf <- list(name = "two_normals_classification_y_random", 
                 type = "classification", 
                 n = n, 
                 y_p = 0.5, 
                 mu0 = rep(0, 10),
                 mu1 = rep(1, 10),
                 sigma0 = diag(10),
                 sigma1 = 2 * diag(10)
)

cvConf <- list(name = "randommxkcv", 
               m = 5, 
               n = n,
               v = 2)



algorConf <- list(name = "classificationTree",
                  type = "classification", 
                  method = "anova")


conf_TASK_VAR_SIM <- list(data_rep_count = 100000, 
                          split_rep_count = 1, 
                          var_for_index = 1,
                          seq_mode = FALSE
                        )