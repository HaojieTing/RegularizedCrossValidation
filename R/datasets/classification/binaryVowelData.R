readVowelAsPopulation<-function(vowelfilename){
  orginalData<-read.table(vowelfilename)
  x<-matrix(NA,nrow(orginalData),ncol(orginalData)-1)
  y_org<-as.vector(orginalData[,1])
  y<-rep(NA,nrow(orginalData))
  x<-data.frame(orginalData[,-c(1,15)])
  y[orginalData[,15]=="had"]<-0
  y[orginalData[,15]=="hed"]<-0
  y[orginalData[,15]=="hid"]<-0
  y[orginalData[,15]=="hod"]<-0
  y[orginalData[,15]=="hud"]<-0
  y[orginalData[,15]=="hAd"]<-1
  y[orginalData[,15]=="hEd"]<-1
  y[orginalData[,15]=="hId"]<-1
  y[orginalData[,15]=="hOd"]<-1
  y[orginalData[,15]=="hUd"]<-1
  v<-which(orginalData[,15]=="hYd")
  o<-sample(v,45)
  y[o]<-0
  y[setdiff(v,o)]<-1
  return(as.data.frame(cbind(x,y)))
}


binaryVowelData.DataGenerator <- function(dataConf){
  n <- dataConf$n
  y_bal <- dataConf$y_balance
  path <- GetDatafilepath('vowel')
  voweldata <- readVowelAsPopulation(path)
  observants <- NULL
  observants <- GenerateObservantsWithReplace(voweldata, n, y_balance = y_bal)
  return(observants)
}

binaryVowelData.Prepackages <- c()


binaryVowelData.validation <- function(dataConf) {
  if(is.null(dataConf$n)) return(FALSE)
  if( is.null(dataConf$y_balance) ) return(FALSE)
  return(TRUE)
} 



vowel.read<-function()
{
  vowel<-read.table("datasetss\\vowel.txt")
  x<-data.frame(vowel[,-c(1,15)])
  y<-vowel[,15]
  return(list(x=x,y=y))
}

vowelTwoClass.read<-function()
{
  vowel<-read.table("datasetss\\vowel.txt")
  x<-data.frame(vowel[,-c(1,15)])
  y<-rep(NA,dim(x)[1])
  y[vowel[,15]=="had"]<-0
  y[vowel[,15]=="hed"]<-0
  y[vowel[,15]=="hid"]<-0
  y[vowel[,15]=="hod"]<-0
  y[vowel[,15]=="hud"]<-0
  y[vowel[,15]=="hAd"]<-1
  y[vowel[,15]=="hEd"]<-1
  y[vowel[,15]=="hId"]<-1
  y[vowel[,15]=="hOd"]<-1
  y[vowel[,15]=="hUd"]<-1
  v<-which(vowel[,15]=="hYd")
  o<-sample(v,45)
  y[o]<-0
  y[setdiff(v,o)]<-1
  y<-factor(y)
  return(list(x=x,y=y))
}