thyroidData.DataGenerator <- function(dataConf) {
  n <- dataConf$n
  path <- GetDatafilepath('thyroid')
  thyroid <-read.csv(path,head=F)
  x<-thyroid[,-1]
  y<-thyroid[,1]
  y<-factor(y)
  population <- as.data.frame(cbind(x, y))      
  return(population)
}


thyroidData.Prepackages <- c()


thyroidData.validataion <- function(dataConf) {
  if( is.null(dataConf$n) ) return(FALSE)
  return(TRUE)
}


