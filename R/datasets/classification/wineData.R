wineData.DataGenerator <- function(dataConf) {
  path <- GetDatafilepath('wine') 
  wine<-read.csv(path, head=F)
  x<-wine[,-1]
  y<-wine[,1]
  return(as.data.frame(cbind(x,y)))
}


wineData.Prepackages <- c()

wineData.validation <- function(dataConf) {
  return(TRUE)
}

comment(wineData.DataGenerator) <- c("")