readVehicleAsPopulation<-function(vehiclefilename){
  orginalData<-read.table(vehiclefilename)
  x<-matrix(NA,nrow(orginalData),ncol(orginalData)-2)
  y<-rep(NA,nrow(orginalData))
  x<-orginalData[,-c(1,20)]
  y[orginalData[,20]=="bus"]<-1
  y[orginalData[,20]=="opel"]<-1
  y[orginalData[,20]=="saab"]<-0
  y[orginalData[,20]=="van"]<-0
  return(list(x,y))
}


binaryVehicleData.DataGenerator <- function(dataConf){
  n <- dataConf$n
  y_bal <- dataConf$y_balance
  path <- GetDatafilepath('vehicle')
  vehicledata <- readVehicleAsPopulation(path)
  observants <- NULL
  observants <- GenerateObservantsWithReplace(vehicledata, n, y_balance = y_bal)
  names(observants) <- c('x', 'y')
  return(observants)
}

binaryVehicleData.Prepackages <- c()


binaryVehicleData.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(FALSE)
  if( is.null(dataConf$y_balance) ) return(FALSE)
  return(TRUE)
} 