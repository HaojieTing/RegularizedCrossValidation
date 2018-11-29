
TASK_COV_SIMU <- "covariance function simulation"
TASK_COV_PARM <- "covariance parameter simulation"
TASK_VAR_SIM  <- "variance simulation"
TASK_VAR_EST  <- "variance estimation"
TASK_TRUE_CI  <- "true confidential interval simulation"
TASK_SIGN_TEST <- "signification test for models"

task <- TASK_SIGN_TEST

temp_dir <- "/apps/users/wrb/trunk/papers/neco_yu/table7_letter_signtest_result/correct_2fcv_t"
temp_dir_var <- "/apps/users/wrb/trunk/papers/neco_yu/table7_letter_signtest_result/2fcv_t"

veConf <- list( name = "sample_variance_estimator_vfcv",
                data_file = file.path(temp_dir, "sign_test", "diff_mu", fsep = .Platform$file.sep),
                v = 2)


conf_TASK_SIGN_TEST <- list(algor1_path = file.path(temp_dir, "variance_simulation_cla_tree", "data", 
                                                    fsep = .Platform$file.sep) ,
                            algor2_path = file.path(temp_dir, "variance_simulation_fnn", "data", fsep = .Platform$file.sep) , 
                            diff_mu_file_name = "diff_mu",
                            test_type = "vfcv_paired_t_test",
                            alpha = 0.05, 
                            freedegree = 1, 
                            true_mu = 0.0, 
                            const = 1, 
                            v = 2,                            
                            rho = 0.5,
                            var_est_conf = veConf)