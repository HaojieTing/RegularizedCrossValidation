readPopulation<-function(filename){
  data<-read.table(filename, sep=",", header=FALSE)
  x<-data[,1:ncol(data)-1]
  y<-data[,ncol(data)]
  y<-y/1000
  data_n <- as.data.frame(cbind(x,y))  
  return(data_n)
}


OnlineNewsPopularity.DataGenerator <- function( dataConf ) {
  n <- dataConf$n  
  path <- GetDatafilepath('OnlineNewsPopularity')
  letterdata <- readPopulation(path)  
  observants <- GenerateObservantsWithReplace(letterdata, n, y_balance = FALSE)
  return(observants)
}


OnlineNewsPopularity.Prepackages <- c()


OnlineNewsPopularity.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(FALSE)
  return(TRUE)
} 