
TASK_COV_SIMU <- "covariance function simulation"
TASK_COV_PARM <- "covariance parameter simulation"
TASK_VAR_SIM  <- "variance simulation"
TASK_VAR_EST  <- "variance estimation"
TASK_TRUE_CI  <- "true confidential interval simulation"
TASK_SIGN_TEST <- "signification test for models"

task <- TASK_SIGN_TEST

temp_dir <- "/apps/users/wrb/trunk/papers/neco_yu/table8_type_1_error/iris/h_size_3/r5x2cv_t"

veConf <- list( name = "var_est_dietterich_mx2cv",
                data_file = file.path(temp_dir, "sign_test", "diff_mu", fsep = .Platform$file.sep),
                m = 5
)

conf_TASK_SIGN_TEST <- list(algor1_path = file.path(temp_dir, "variance_simulation_nnet_0.6", "data", 
                                                    fsep = .Platform$file.sep) ,
                            algor2_path = file.path(temp_dir, "variance_simulation_nnet_0.7", "data", 
                                                    fsep = .Platform$file.sep) , 
                            diff_mu_file_name = "diff_mu",
                            test_type = "Dietterich_t_test",
                            alpha = 0.05, 
                            freedegree = 5, 
                            true_mu = 0.0, 
                            const = 1, 
                            m = 5,
                            numeratorIndex = 1,
                            var_est_conf = veConf)