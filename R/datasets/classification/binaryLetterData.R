readLetterAsPopulation<-function(letterfilename){
  orginalData<-read.table(letterfilename)
  x<-matrix(NA,nrow(orginalData),ncol(orginalData)-1)
  y_org<-as.vector(orginalData[,1])
  y<-rep(NA,nrow(orginalData))
  for(i in 1:ncol(orginalData)-1){
    x[,i]<-orginalData[,i+1]
  }
  for(i in 1:nrow(orginalData)){
    if(y_org[i]<='M') 
      y[i] <- 0
    else 
      y[i]<-1
  }
  data <- as.data.frame(cbind(x,y))
  data[,ncol(data)] <- factor(data[,ncol(data)])
  return(data)
}


binaryLetterData.DataGenerator <- function( dataConf ) {
  n <- dataConf$n
  y_bal <- dataConf$y_balance
  path <- GetDatafilepath('letter')
  letterdata <- readLetterAsPopulation(path)
  observants <- NULL
  observants <- GenerateObservantsWithReplace(letterdata, n, y_balance = y_bal)
  return(observants)
}


binaryLetterData.Prepackages <- c()


binaryLetterData.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(FALSE)
  if( is.null(dataConf$y_balance) ) return(FALSE)
  return(TRUE)
} 