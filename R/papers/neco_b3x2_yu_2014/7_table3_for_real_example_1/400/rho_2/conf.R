TASK_COV_SIMU <- "covariance function simulation"
TASK_COV_PARM <- "covariance parameter simulation"
TASK_VAR_SIM  <- "variance simulation"
TASK_VAR_EST  <- "variance estimation"
TASK_TRUE_CI  <- "true confidential interval simulation"
TASK_SIGN_TEST <- "signification test for models"

task <- TASK_COV_SIMU

n <- 400

dataConf <- list(name = "binaryLetterData", 
                 type = "classification", 
                 n = n                 
)

cvConf <- list(name = "held_out",                
               n = n,
               n1 = n/2)

algorConf <- list(name = "classificationTree",
                  type = "classification",
                  method = "anova"
)

conf_TASK_COV_SIMU <- list(exchange_count = n/4,
                           rpt = 100000,
                           n1 = n/2,
                           seq_mode = FALSE)