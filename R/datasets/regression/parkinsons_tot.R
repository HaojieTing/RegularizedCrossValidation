readPopulation<-function(filename){
  data<-read.table(filename, sep=",", header=FALSE)
  x<-data[,1:ncol(data)-1]
  y<-data[,ncol(data)]
  data_n <- as.data.frame(cbind(x,y))  
  return(data_n)
}


parkinsons_tot.DataGenerator <- function( dataConf ) {
  n <- dataConf$n  
  path <- GetDatafilepath('parkinsons_tot')
  letterdata <- readPopulation(path)  
  observants <- GenerateObservantsWithReplace(letterdata, n, y_balance = FALSE)
  return(observants)
}


parkinsons_tot.Prepackages <- c()


parkinsons_tot.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(FALSE)
  return(TRUE)
} 