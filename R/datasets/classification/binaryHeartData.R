readHeartAsPopulation<-function(heart_statlogfilename){
  orginalData<-read.table(heart_statlogfilename)
  x<-matrix(NA,nrow(orginalData),ncol(orginalData)-1)
  y_org<-as.vector(orginalData[,1])
  y<-rep(NA,nrow(orginalData))
  x<-orginalData[,1:13]
  y[orginalData[,14]=="absent"]<-0
  y[orginalData[,14]=="present"]<-1
  return(as.data.frame(cbind(x,y)))
}


binaryHeartData.DataGenerator <- function(dataConf){
  n <- dataConf$n
  y_bal <- dataConf$y_balance
  path <- GetDatafilepath('heart_statlog')
  heartdata <- readHeartAsPopulation(path)
  observants <- NULL
  observants <- GenerateObservantsWithReplace(heartdata, n, y_balance = y_bal)
  return(observants)
}

binaryHeartData.Prepackages <- c()

binaryHeartData.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(FALSE)
  return(TRUE)
} 