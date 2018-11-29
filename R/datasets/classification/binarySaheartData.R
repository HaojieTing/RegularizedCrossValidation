readSaheartAsPopulation<-function(saheartfilename){
  orginalData<-read.table(saheartfilename)
  x<-matrix(NA,nrow(orginalData),ncol(orginalData)-1)
  y_org<-as.vector(orginalData[,1])
  y<-rep(NA,nrow(orginalData))
  x<-orginalData[,1:9]
  y[orginalData[,10]==1]<-0
  y[orginalData[,10]==2]<-1
  return(list(x,y))
}


binarySaheartData.DataGenerator <- function(dataConf){
  n <- dataConf$n
  y_bal <- dataConf$y_balance
  path <- GetDatafilepath('saheart')
  saheartdata <- readSaheartAsPopulation(path)
  observants <- NULL
  observants <- GenerateObservantsWithReplace(saheartdata, n, y_balance = y_bal)
  names(observants) <- c('x', 'y')
  return(observants)
}

binarySaheartData.Prepackages <- c()

binarySaheartData.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(FALSE)
  return(TRUE)
}

