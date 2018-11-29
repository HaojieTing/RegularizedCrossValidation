TASK_COV_SIMU <- "covariance function simulation"
TASK_COV_PARM <- "covariance parameter simulation"
TASK_VAR_SIM  <- "variance simulation"
TASK_VAR_EST  <- "variance estimation"
TASK_TRUE_CI  <- "true confidential interval simulation"
TASK_SIGN_TEST <- "signification test for models"

task <- TASK_VAR_SIM

n <- 1024

dataConf <- list(name = "two_class_YU_example_2_1", 
                 type = "classification", 
                 n = n, 
                 d = 300
)

cvConf <- list(name = "balancedmx2cv", 
               m = 3, 
               n = n)



algorConf <- list(name = "classificationTree",
                  type = "classification", 
                  method = "anova")


conf_TASK_VAR_SIM <- list(data_rep_count = 100000, 
                          split_rep_count = 100, 
                          var_for_index = 0,
                          seq_mode = FALSE
)