
vowelData.DataGenerator <- function(dataConf){
  path <- GetDatafilepath('vowel')
  vowel <- read.table(path) 
  x<-data.frame(vowel[,-c(1,15)])
  y<-vowel[,15]
  population <- as.data.frame(cbind(x, y))  
  return(population)
}


vowelData.Prepackages <- c()


vowelData.validation <- function(dataConf) {
  if( is.null(dataConf$n) ) return(FALSE)
  return(TRUE)
}
