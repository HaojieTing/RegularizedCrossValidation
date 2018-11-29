readIrisAsPopulation<-function(irisfilename) {
  orginalData<-read.table(irisfilename)
  x<-matrix(NA,nrow(orginalData), ncol(orginalData)-1)
  y<-rep(NA,150)
  x<-orginalData[,1:4]
  y[1:50]<-1
  y[51:100]<-0
  o<-sample(c(101:150), 25)
  y[o]<-1
  y[setdiff(c(101:150),o)]<-0
  return(as.data.frame(cbind(x,y)))
}


binaryIrisData.DataGenerator <- function(dataConf ){
  n <- dataConf$n
  y_bal <- dataConf$y_balance
  path <- GetDatafilepath('iris')
  irisdata <- readIrisAsPopulation(path)
  observants <- NULL
  observants <- GenerateObservantsWithReplace(irisdata, n, y_balance = y_bal)
  return(observants)
}


binaryIrisData.Prepackages <- c()


binaryIrisData.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(FALSE)
  if( is.null(dataConf$y_balance) ) return(FALSE)
  return(TRUE)
} 