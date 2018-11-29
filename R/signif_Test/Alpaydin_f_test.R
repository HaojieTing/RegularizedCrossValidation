numerator.compute <- function(muvector, m, true_mu) {
  muvector <- muvector - true_mu
  if ( length(muvector) != (2*m+1) ) {
    stop("muvector")
  }
  muvector <- muvector[1:(2*m)]
  return(c(muvector %*% muvector))
}


Alpaydin_f_test.process <- function(testConf) {
  
  load(testConf$algor1_path)
  mumat_algor1 <- muv
  rm(muv)
  load(testConf$algor2_path)
  mumat_algor2 <- muv
  rm(muv)
  z <- normalized_distance_error(mumat_algor1, mumat_algor2)
  if(nrow(mumat_algor1) != nrow(mumat_algor2) || ncol(mumat_algor1) != ncol(mumat_algor2))
      stop("")
  
  true_mu <- testConf$true_mu    
  t_alpha <- testConf$alpha
  freedegree <- testConf$freedegree
  freedegree2 <- testConf$freedegree2
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
  f_value <- qf(1- t_alpha , freedegree, freedegree2)  
  muvec <-apply(muv, 1, numerator.compute, m, true_mu)
  if(length(muvec) != length(var_est_vec)) {
    stop("")
  }
  test_result <- muvec / (2* m* var_est_vec) > const * f_value
  errorProb <- mean(test_result, na.rm = TRUE)
  sigmasqr <- NULL
  if(!is.null(testConf$true_mu_algor1_filename) && !is.null(testConf$true_mu_algor2_filename)) {
     load(testConf$true_mu_algor1_filename)
     mu_a1 <- muv
     rm(muv)
     load(testConf$true_mu_algor2_filename)
     mu_a2 <- muv
     rm(muv)
     mu_a1_a2 <- mu_a1 - mu_a2
     cols <- ncol(mu_a1_a2)
     vars <- rep(NA, cols-1)
     for(i in 1:(cols-1))
       vars[i] <- var(mu_a1_a2[,i])
     sigmasqr <- mean(vars)
  } else {
    
    sigmasqr <- mean(var_est_vec)
  }
  pot_start_value <- NULL
  pot_end_value <- NULL
  pot_step_value <- NULL
  if( is.null(testConf$pot_int) ) {
    pot_start_value <- 0
    pot_end_value <- 1
    pot_step_value <- 0.001
  } else {
    pot_start_value <- testConf$pot_int$start
    pot_end_value <- testConf$pot_int$end
    pot_step_value <- testConf$pot_int$step
  }
  x_var <- seq(from = pot_start_value, to = pot_end_value, by = pot_step_value)    
  y <- 1 -  pf(f_value, freedegree, freedegree2, m * x_var^2 /sigmasqr)   
  potential_mat <- matrix(NA, nrow = 2, ncol = length(x_var))
  potential_mat[1,] <- x_var
  potential_mat[2,] <- y
  return(list(typeI_error = errorProb, potential = potential_mat, test_result = test_result, z = z))  
}


Alpaydin_f_test.validate <- function(testConf) {
  if(is.null(testConf$true_mu)) return(FALSE)
  if(is.null(testConf$alpha))   return(FALSE)
  if(is.null(testConf$const))   return(FALSE)
  if(is.null(testConf$freedegree)) return(FALSE)
  if(is.null(testConf$freedegree2)) return(FALSE)
  if(is.null(testConf$m))       return(FALSE)
  if(is.null(testConf$var_est_conf)) return(FALSE)
  if(is.null(testConf$diff_mu_file_name)) return(FALSE)   
  return(TRUE)
}