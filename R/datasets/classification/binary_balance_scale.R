balance_scale.DataGenerator <- function(dataConf){
  n <- dataConf$n
  path <- GetDatafilepath('balance_scale')
  balance<-read.table(path)
  x<-balance[,-5]
  y<-balance[,5]
  list.population <- list(x, y)
  list.xy <- GenerateObservantsWithReplace(list.population, n)
  names(list.xy) <- c('x', 'y')
  return(list.xy)
}


balance_scale.Prepackages <- c()


balance_scale.validation <- function(dataConf) {
  if( is.null(dataConf$n) ) return(FALSE)
  return(TRUE)
}
