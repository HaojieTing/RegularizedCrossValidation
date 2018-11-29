readBupaAsPopulation<-function(bupafilename){
  orginalData<-read.table(bupafilename)
  x<-matrix(NA,nrow(orginalData),ncol(orginalData)-1)
  y<-rep(NA,nrow(orginalData))
  x<-orginalData[,1:6]
  y[orginalData[,7]==1]<-0
  y[orginalData[,7]==2]<-1
  return(as.data.frame(cbind(x,y)))
}


binaryBupaData.DataGenerator <- function(dataConf){
  n <- dataConf$n
  y_bal <- dataConf$y_balance
  path <- GetDatafilepath('bupa')
  bupadata <- readBupaAsPopulation(path)
  observants <- NULL
  observants <- GenerateObservantsWithReplace(bupadata, n, y_balance = y_bal)  
  return(observants)
}

binaryBupaData.Prepackages <- c()

binaryBupaData.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(FALSE)
  return(TRUE)
} 