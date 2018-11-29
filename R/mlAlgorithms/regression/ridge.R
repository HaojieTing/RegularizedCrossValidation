# 岭回归算法。
# 
# MASS包中自带的lm.ridge算法，尽管可以训练，但是在测试时非常不方便。
# 因此，这里，我们使用ridge包中的linearRidge算法。
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/6/6
#
# TODO: wangruibo@2017/6/6: ridge包依赖于R-3以上的版本。


# lmRidge.fit<-function(data_train, algorConf) {
#     lambda <- 1
#     if(!is.null(algorConf$lambda)) lambda <- algorConf$lambda
#     fomu <- as.formula(paste(colnames(data_train)[ncol(data_train)], '~.', sep=""))    
#     return(lm.ridge(fomu, data=data_train, lambda = lambda))
# }
# 
# 
# lmRidge.predict<-function(fit, data_test, algorConf){  
#   x <- as.matrix(data_test)
#   x <- x[,-ncol(x)]
#   xcol = ncol(x)
#   if(is.null(xcol) )
#     xcol=length(x)
#   if(xcol!= length(fit$coef)) {
#     if(length(fit$coef) - xcol == 1) {
#        x <- cbind(1, x)
#     } else {
#       stop("length problem")
#     }
#   }
#   return(c(x%*%fit$coef))
# }
# 
# 
# lmRidgeModel.TrainAndTest <- function(data_train, data_test, algorConf) {
#   model <- lmRidge.fit(data_train, algorConf)
#   pre <- lmRidge.predict(model, data_test, algorConf)
#   return(pre)
# }
# 
# 
# lmRidgeModel.Prepackages <- c("ridge")



lmRidgeModel.validation <- function(algorConf) {
  return(TRUE)
}