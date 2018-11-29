readIonosphereAsPopulation<-function(ionospherefilename){
  orginalData<-read.table(ionospherefilename)
  x<-matrix(NA,nrow(orginalData),ncol(orginalData)-2)
  y<-rep(NA,nrow(orginalData))
  x<-ionosphere[,-c(2,35)]
  y<-orginalData[,35]
  y[orginalData[,35]=="g"]<-0
  y[orginalData[,35]=="b"]<-1
  return(list(x,y))
}


binaryIonosphereData.DataGenerator <- function(dataConf){
  n <- dataConf$n
  y_bal <- dataConf$y_balance
  path <- GetDatafilepath('ionosphere')
  ionospheredata <- readIonosphereAsPopulation(path)
  observants <- NULL
  observants <- GenerateObservantsWithReplace(ionospheredata, n, y_balance = y_bal)
  names(observants) <- c('x', 'y')
  return(observants)
}

binaryIonosphereData.Prepackages <- c()


binaryIonosphereData.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(FALSE)
  if( is.null(dataConf$y_balance) ) return(FALSE)
  return(TRUE)
} 