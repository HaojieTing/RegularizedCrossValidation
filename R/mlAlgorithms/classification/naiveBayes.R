# 朴素贝叶斯分类器
#
# 
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/6/3

nb.fit<-function(data_train, algorConf) {
  colnames(data_train)[ncol(data_train)] <- "y"
  fit<-naiveBayes(as.factor(y)~.,data=data_train)
  return(fit)
}


nb.predict<-function(fit, x, algorConf){
  pre<-stats::predict(fit,x,type="class")
  return(pre)
}



naiveBayes.TrainAndTest <- function(data_train, xtest, algorConf) {
  model <- nb.fit(data_train, algorConf)
  pre <-nb.predict (model, xtest, algorConf)
  return(pre)
}



naiveBayes.Prepackages <- c("e1071")


naiveBayes.validation <- function(algorConf) {
  return(TRUE)
}