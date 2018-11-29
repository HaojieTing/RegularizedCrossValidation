readPopulation<-function(filename){
  data<-read.table(filename, sep=",", header=FALSE)
  x<-data[,1:ncol(data)-1]
  y<-data[,ncol(data)]
  data_n <- as.data.frame(cbind(x,y))  
  return(data_n)
}


wine_quality_white.DataGenerator <- function( dataConf ) {
  n <- dataConf$n  
  path <- GetDatafilepath('wine_quality_white')
  letterdata <- readPopulation(path)  
  observants <- GenerateObservantsWithReplace(letterdata, n, y_balance = FALSE)
  return(observants)
}


wine_quality_white.Prepackages <- c()


wine_quality_white.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(FALSE)
  return(TRUE)
} 