readBalanceAsPopulation<-function(balance_scalefilename){
  orginalData<-read.table(balance_scalefilename)
  x<-matrix(NA,nrow(orginalData),ncol(orginalData)-1)
  y_org<-as.vector(orginalData[,1])
  y<-rep(NA,nrow(orginalData))
  x<-orginalData[,-5]
  y[orginalData[,5]=="L"]<-0
  y[orginalData[,5]=="R"]<-1
  v<-which(orginalData[,5]=="B")
  o<-sample(v,25)
  y[o]<-0
  y[setdiff(v,o)]<-1
  return(as.data.frame(cbind(x,y)))
}


binaryBalanceData.DataGenerator <- function(dataConf){
  n <- dataConf$n
  y_bal <- dataConf$y_balance
  path <- GetDatafilepath('balance_scale')
  balancedata <- readBalanceAsPopulation(path)
  observants <- NULL
  observants <- GenerateObservantsWithReplace(balancedata, n, y_balance = y_bal)  
  return(observants)
}


binaryBalanceData.Prepackages <- c()


binaryBalanceData.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(FALSE)
  if( is.null(dataConf$y_balance) ) return(FALSE)
  return(TRUE)
} 

