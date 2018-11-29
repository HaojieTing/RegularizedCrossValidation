readDisbetesAsPopulation<-function(disbetesfilename){
  orginalData<-read.table(disbetesfilename)
  x<-matrix(NA,nrow(orginalData),ncol(orginalData)-1)
  y<-rep(NA,nrow(orginalData))
  x<-orginalData[,1:8]
  orginalData[,9]
  y[orginalData[,9]=="tested_negative"]<-1
  y[orginalData[,9]=="tested_positive"]<-0
  ####500个1,268个0
  return(list(x,y))
}


binaryDisbetesData.DataGenerator <- function(n, ...){
  n <- dataConf$n
  y_bal <- dataConf$y_balance
  path <- GetDatafilepath('disbetes')
  disbetesdata <- readDisbetesAsPopulation(path)
  observants <- NULL
  observants <- GenerateObservantsWithReplace(disbetesdata, n, y_balance = y_bal)
  names(observants) <- c('x', 'y')
  return(observants)
}


binaryDisbetesData.Prepackages <- c()


binaryDisbetesData.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(FALSE)
  if( is.null(dataConf$y_balance) ) return(FALSE)
  return(TRUE)
} 
