GaussianMixtureData.DataGenerator <- function(number, d){
  p= 0.5
  require(MASS)
  y <- rep(NA, number)
  mean <- rep(0, d)
  cov <- diag(d)
  x <- matrix(rep(NA,number *d), nrow=number)
  for(i in 1:number){
    y[i] <- rbinom(1,1, p)    
    if(y[i] == 1)
    {
      x[i,] <- mvrnorm(1, mean, cov )
    }else{
      x[i,] <- mvrnorm(1, mean, 2*cov)
      
    }
  }
  return(list(x,y))
}
