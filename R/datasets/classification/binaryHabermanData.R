readHabermanAsPopulation<-function(habermanfilename){
  orginalData<-read.table(habermanfilename,sep = ",")
  x<-matrix(NA,nrow(orginalData),ncol(orginalData)-1)
  y<-rep(NA,nrow(orginalData))
  x<-orginalData[,1:3]
  y[orginalData[,4]=="negative"]<-0
  y[orginalData[,4]=="positive"]<-1
  return(list(x,y))
}


binaryHabermanData.DataGenerator <- function(dataConf){
  n <- dataConf$n
  y_bal <- dataConf$y_balance
  path <- GetDatafilepath('haberman')
  habermandata <- readHabermanAsPopulation(path)
  observants <- NULL
  observants <- GenerateObservantsWithReplace(habermandata, n, y_balance = y_bal)
  names(observants) <- c('x', 'y')
  return(observants)
}

binaryHabermanData.Prepackages <- c()

binaryHabermanData.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(FALSE)
  return(TRUE)
} 