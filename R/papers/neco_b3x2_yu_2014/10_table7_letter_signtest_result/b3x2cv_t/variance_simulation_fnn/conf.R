TASK_COV_SIMU <- "covariance function simulation"
TASK_COV_PARM <- "covariance parameter simulation"
TASK_VAR_SIM  <- "variance simulation"
TASK_VAR_EST  <- "variance estimation"
TASK_TRUE_CI  <- "true confidential interval simulation"
TASK_SIGN_TEST <- "signification test for models"

task <- TASK_VAR_SIM

n <- 300

dataConf <- list(name = "binaryLetterData", 
                 type = "classification",
                 n = n
)

cvConf <- list(name = "balancedmx2cv", 
               m = 3, 
               n = n)

algorConf <- list(name = "firstNearestNeighborhood", 
                  type = "classification",
                  w = 25)

conf_TASK_VAR_SIM <- list(data_rep_count = 100000, 
                          split_rep_count = 1, 
                          seq_mode = FALSE, 
                          var_for_index = 0
)