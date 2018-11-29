TASK_COV_SIMU <- "covariance function simulation"
TASK_COV_PARM <- "covariance parameter simulation"
TASK_VAR_SIM  <- "variance simulation"
TASK_VAR_EST  <- "variance estimation"
TASK_TRUE_CI  <- "true confidential interval simulation"
TASK_SIGN_TEST <- "signification test for models"

task <- TASK_COV_SIMU

n <- 512

dataConf <- list(name = "two_class_with_sigmoid_prob_YU_exampleII", 
                 type = "classification", 
                 n = n                 
)

cvConf <- list(name = "held_out",                
               n = n,
               n1 = n/2)

algorConf <- list(name = "svm",
                  type = "classification"
                )

conf_TASK_COV_SIMU <- list(exchange_count = 190,
                           rpt = 100000,
                           seq_mode = FALSE)