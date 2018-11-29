SimpleNormalDataSet_otherimpl.DataGenerator <- function(dataConf){
  n <- dataConf$n
  x <- rnorm(n, 10, 1)
  rnrm <- function(mne, var) return(rnorm(1, mne, var))
  y <- as.numeric(lapply(100+0.1*x ,rnrm, sqrt(9.97)))
  return(list(x = as.matrix(x), y = y))
}

SimpleNormalDataSet_otherimpl.Prepackages <- c()


SimpleNormalDataSet_otherimpl.Validation <- function(dataConf) {
  if( is.null(dataConf$n) ) return(FALSE)
  return(TRUE)
}


