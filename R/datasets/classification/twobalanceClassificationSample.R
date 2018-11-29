twobalanceClassificationSample.DataGenerator<-function(dataConf){
  n <- dataConf$n
  d <- dataConf$d
  signal <- dataConf$signal
  require(MASS)
  if(n %%2 != 0)
    stop("To ensure the prioris of two classes are equal, the even n is only supported.")
  x <- NA
  y <- NA
  if(!signal){ #no signal
    cov<-diag(d)
    mean <- rep(0, d)
    x <- mvrnorm(n, mean, cov)
    y <- append(rep(0,n/2),rep(1, n/2))
  }else{ #signal
    cov<-diag(d)
    mean1 <- rep(0, d)
    mean2 <- append(rep(0.5,round(d/10)),rep(0, d-round(d/10)))
    x1 <- mvrnorm(n/2, mean1, cov)
    x2 <- mvrnorm(n/2, mean2, cov)
    x  <- rbind(x1,x2)
    y <- append(rep(0,n/2),rep(1, n/2))
  }   
  order <- sample(n)
  y <- y[order]
  x <- x[order,]
  data <- as.data.frame(cbind(x, y = y))
  data[,ncol(data)] <- factor(data[,ncol(data)])
  return(data)
}


twobalanceClassificationSample.Prepackages <- c("MASS")


twobalanceClassificationSample.validation <- function(dataConf) {
  if( is.null(dataConf$n) ) return(FALSE)
  if( is.null(dataConf$d) ) return(FALSE)
  if( is.null(dataConf$signal) ) return(FALSE)
  return(TRUE)
}


comment(x = twobalanceClassificationSample.DataGenerator) <- c()