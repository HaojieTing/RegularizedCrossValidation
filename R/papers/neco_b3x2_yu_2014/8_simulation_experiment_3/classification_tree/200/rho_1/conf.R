TASK_COV_SIMU <- "covariance function simulation"
TASK_COV_PARM <- "covariance parameter simulation"
TASK_VAR_SIM  <- "variance simulation"
TASK_VAR_EST  <- "variance estimation"
TASK_TRUE_CI  <- "true confidential interval simulation"
TASK_SIGN_TEST <- "signification test for models"

task <- TASK_COV_SIMU

n <- 200

dataConf <- list(name = "two_normals_classification_y_random", 
                 type = "classification", 
                 n = n, 
                 y_p = 0.5, 
                 mu0 = rep(0, 5),
                 mu1 = rep(1, 5),
                 sigma0 = diag(5),
                 sigma1 = 2 * diag(5)
)


cvConf <- list(name = "held_out",                
               n = n,
               n1 = n/2)

algorConf <- list(name = "classificationTree",
                  type = "classification",
                  method = "anova")


conf_TASK_COV_SIMU <- list(exchange_count = n/2,
                           rpt = 100000,
                           n1 = n/2,
                           seq_mode = FALSE)