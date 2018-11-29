TASK_COV_SIMU <- "covariance function simulation"
TASK_COV_PARM <- "covariance parameter simulation"
TASK_VAR_SIM  <- "variance simulation"
TASK_VAR_EST  <- "variance estimation"
TASK_TRUE_CI  <- "true confidential interval simulation"
TASK_SIGN_TEST <- "signification test for models"

task <- TASK_COV_SIMU

n <- 20

dataConf <- list(name = "regrDataBG_nooutliers", 
                 type = "regression", 
                 n = n,
                 d = 3
)

cvConf <- list(name = "held_out",                
               n = n,
               n1 = n/2)

algorConf <- list(name = "linearModel",
                  type = "regression",
                  no_intercept = FALSE
)

conf_TASK_COV_SIMU <- list(exchange_count = 7,
                           rpt = 100000,
                           n1 = n %/% 2,
                           seq_mode = FALSE)