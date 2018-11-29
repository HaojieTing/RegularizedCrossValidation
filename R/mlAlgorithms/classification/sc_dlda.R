sc_dlda.TrainAndTest <- function(data_train, data_test, algorConf) {
  return(dlda(data_train[,1:(ncol(data_train)-1)],data_test,data_train[,ncol(data_train)])) 
}

sc_dlda.Prepackages <- c("supclust")


sc_dlda.validation <- function(algorConf)  {
  return(TRUE)
}
