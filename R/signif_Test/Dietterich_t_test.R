Dietterich_t_test.process <- function(testConf) {
  load(testConf$algor1_path)
  mumat_algor1 <- muv
  rm(muv)
  load(testConf$algor2_path)
  mumat_algor2 <- muv
  rm(muv)
  z <- normalized_distance_error(mumat_algor1, mumat_algor2)
  if(nrow(mumat_algor1) != nrow(mumat_algor2) || ncol(mumat_algor1) != ncol(mumat_algor2)) {
      stop("")
  }  
  
  true_mu <- testConf$true_mu  
  var_est_vec <- NULL  
  t_alpha <- testConf$alpha
  freedegree <- testConf$freedegree 
  const <- testConf$const
  m <- testConf$m
  numeratorIndex <- testConf$numeratorIndex
  muv <- mumat_algor1-mumat_algor2
  diff_mu_file <- testConf$diff_mu_file_name
  save(muv, file=paste(data_dir, diff_mu_file, sep=.Platform$file.sep))
  
  veConf <- testConf$var_est_conf
  validate_status <- validateVarEstConf(veConf)
  if( !validate_status ) {
    stop("configuration of variance estimation is incorrect.[VE]")
  }
  ve.estimator <- loadVarEstInfo(vename = veConf$name)
  var_est_vec <- ve.estimator(veConf)
  
  t_value <- qt(1-t_alpha/2, freedegree) 
  
  muvec <- muv[,numeratorIndex]
  test_result <- abs( muvec - true_mu) / sqrt(var_est_vec) > const * t_value  
  errorProb <- mean(test_result, na.rm = TRUE)
  temp_alpha <- t_alpha
  temp_error_prob <- errorProb  
  
  pot_start_value <- NULL
  pot_end_value <- NULL
  pot_step_value <- NULL
  if( is.null(testConf$pot_int) ) {
    pot_start_value <- -1
    pot_end_value <- 1
    pot_step_value <- 0.001
  } else {
    pot_start_value <- testConf$pot_int$start
    pot_end_value <- testConf$pot_int$end
    pot_step_value <- testConf$pot_int$step
  }
  x_var <- seq(from = pot_start_value, to = pot_end_value, by = pot_step_value)  
  y <- 1-pt(const * t_value-x_var/(sqrt(mean(var_est_vec))), freedegree)+pt(-const * t_value-x_var/(sqrt(mean(var_est_vec))),freedegree)
  potential_mat <- matrix(NA, nrow = 2, ncol = length(x_var))
  potential_mat[1,] <- x_var
  potential_mat[2,] <- y
  return(list(typeI_error = errorProb, potential = potential_mat, test_result = test_result, z = z))  
}


Dietterich_t_test.validate <- function(testConf) {
  if(is.null(testConf$true_mu)) return(FALSE)
  if(is.null(testConf$alpha))   return(FALSE)
  if(is.null(testConf$const))   return(FALSE)
  if(is.null(testConf$freedegree)) return(FALSE)
  if(is.null(testConf$m))       return(FALSE)
  if(is.null(testConf$var_est_conf)) return(FALSE)
  if(is.null(testConf$diff_mu_file_name)) return(FALSE)
  if(is.null(testConf$numeratorIndex)) return(FALSE)  
  return(TRUE)
}