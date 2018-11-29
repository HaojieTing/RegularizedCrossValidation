two_class_YU_example_2_1.DataGenerator <- function( dataConf ) {
  n <- dataConf$n
  d <- dataConf$d
  if( n %% 2 != 0) {
    stop("n must be divided by 2")
  }
  if( d %% 3 != 0) {
    stop("d must be divided by 3")
  }
  x <- matrix(NA, nrow = n, ncol = d)
  for (i in 1:n) {
    if ( i <= n/2 ) {
      x[i,] <- rnorm(n = d, mean = 0, sd = 1)
    } else {
      d_factor <- d /3      
      x[i, 1:d_factor] <- rnorm(n = d_factor, mean = 0.4, sd = 1)
      x[i, (d_factor + 1) : (2 * d_factor)] <- rnorm(n = d_factor , mean = 0.3, sd = 1)
      x[i, (2 * d_factor + 1):d] <- rnorm(n = d_factor, mean=0, sd = 1)
    }
  }
  y <- c(rep(1, n/2), rep(0, n/2))
  data <- as.data.frame(cbind(x, y))
  data <- data[sample(1:n, n,replace = FALSE),]
  data[,ncol(data)] <- factor(data[,ncol(data)])
  return(data)
}


two_class_YU_example_2_1.Prepackages <- c()


two_class_YU_example_2_1.validation <- function( dataConf ) {
  if(is.null(dataConf$n)) return(FALSE)
  if(is.null(dataConf$d)) return(FALSE)
  return(TRUE)
}


