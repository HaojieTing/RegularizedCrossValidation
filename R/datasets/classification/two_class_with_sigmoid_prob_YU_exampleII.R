two_class_with_sigmoid_prob_YU_exampleII.DataGenerator <- function(dataConf) {
  n <- dataConf$n
  x <- matrix(rnorm(n = 5 * n, mean = 0, sd = 1), nrow = n, ncol = 5)
  y <- rep(NA, n)
  coeff.value <- 1.5
  if(!is.null(dataConf$coef))
    coeff.value <- dataConf$coef
  coeff <- rep(coeff.value, 5)
  for( i in 1:n) {
    affine <- x[i, ] %*% coeff
    prob <- exp(affine + 1) /( 1 + exp(affine + 1))
    y[i] <- rbinom(n = 1, size = 1, prob)
  }
  data <- as.data.frame(cbind(x, y))
  data[,ncol(data)] <- factor(data[,ncol(data)])
  return(data)
}


two_class_with_sigmoid_prob_YU_exampleII.Prepackages <- c()


two_class_with_sigmoid_prob_YU_exampleII.validation <- function(dataConf) {
  if( is.null(dataConf$n) ) return(FALSE)
  return(TRUE)
}
