corrected_vfcv_paired_t_test.process <- function(testConf) {
  load(testConf$algor1_path)
  mumat_algor1 <- muv
  rm(muv)
  load(testConf$algor2_path)
  mumat_algor2 <- muv
  rm(muv)
  z <- normalized_distance_error(mumat_algor1, mumat_algor2)
  if(nrow(mumat_algor1) != nrow(mumat_algor2) || ncol(mumat_algor1) != ncol(mumat_algor2))
    stop("算法1和算法2对应的mu矩阵大小不同！")
  #加载参数
  true_mu <- testConf$true_mu
  alpha <- testConf$alpha
  rho <- testConf$rho
  freedegree <- testConf$freedegree
  v <- testConf$v
  const <- testConf$const
  muv <- mumat_algor1-mumat_algor2
  diff_mu_file <- testConf$diff_mu_file_name
  save(muv, file=paste(data_dir, diff_mu_file, sep=.Platform$file.sep))
  veConf <- testConf$var_est_conf
  validate_status <- validateVarEstConf(veConf)
  if( !validate_status ) {
    stop("configuration of variance estimation is incorrect.[VE]")
  }
  ve.estimator <- loadVarEstInfo(vename = veConf$name)
  var_est_vec  <- ve.estimator(veConf)
  t_value <- qt(1 - alpha/2, freedegree) 
  
  muvec <- muv[,ncol(muv)]
  t_test_value = abs( muvec - true_mu) / sqrt(var_est_vec / ((1-rho)/(1+rho)))
  t_test_value[is.infinite(t_test_value)] <- t_value * const + 1
  t_test_value[is.na(t_test_value)] <- t_value * const + 1
  test_result <- t_test_value > const * t_value
  errorProb <- mean(test_result)
  temp_alpha <- alpha
  temp_error_prob <- errorProb
  if( is.null(testConf$rect_potential) || TRUE == testConf$rect_potential) {}
	#计算势函数
  x_var <- seq(from = -1, to = 1, by = 0.02)
  y <- 1-pt(const * t_value-x_var/(sqrt(mean(var_est_vec))), freedegree)+pt(-const * t_value-x_var/(sqrt(mean(var_est_vec))),freedegree)
  potential_mat <- matrix(NA, nrow = 2, ncol = length(x_var))
  potential_mat[1,] <- x_var
  potential_mat[2,] <- y
  return(list(typeI_error = errorProb, potential = potential_mat, test_result = test_result, z = z))  
}

corrected_vfcv_paired_t_test.validate <- function(testConf) {
  if(is.null(testConf$true_mu)) return(FALSE)
  if(is.null(testConf$alpha)) return(FALSE)
  if(is.null(testConf$freedegree)) return(FALSE)
  if(is.null(testConf$v)) return(FALSE)
  if(is.null(testConf$rho)) return(FALSE)
  if(is.null(testConf$const)) return(FALSE)
  if(is.null(testConf$var_est_conf)) return(FALSE)  
  return(TRUE)
}