uci_vowel.DataGenerator <- function(dataConf){
  vowel.data.set <- GetExternalDataSet('vowel', header=F)
   
  vowel.x<-vowel.data.set[,-c(1,15)]
  vowel.y<-vowel.data.set[,15]
  vowel.y <- factor(vowel.y)
  population <- as.data.frame(cbind(x, y))  
  vowel.data.set <- as.data.frame(cbind(vowel.x, vowel.y))
  return(vowel.data.set)
}


uci_vowel.Prepackages <- c()


uci_vowel.validation <- function(dataConf) {
  return(TRUE)
}