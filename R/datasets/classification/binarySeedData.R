readSeedAsPopulation<-function(seedfilename){
  orginalData<-read.table(seedfilename)
  x<-matrix(NA,nrow(orginalData),ncol(orginalData)-1)
  y<-rep(NA,nrow(orginalData))
  x<-orginalData[,1:7]
  y[1:70]<-1
  y[71:140]<-0
  o<-sample(c(141:210),35)
  y[o]<-1
  y[setdiff(c(141:210),o)]<-0
  return(as.data.frame(cbind(x,y)))
}


binarySeedData.DataGenerator <- function(dataConf){
  n <- dataConf$n
  y_bal <- dataConf$y_balance
  path <- GetDatafilepath('seed')
  seeddata <- readSeedAsPopulation(path)
  observants <- NULL
  observants <- GenerateObservantsWithReplace(seeddata, n, y_balance = y_bal)
  return(observants)
}


binarySeedData.Prepackages <- c()


binarySeedData.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(FALSE)
  if( is.null(dataConf$y_balance) ) return(FALSE)
  return(TRUE)
}