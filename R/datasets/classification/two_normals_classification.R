# 数据来自:
#     Nadeau, C., & Bengio, Y. (2003). Inference for the Generalization Error. Machine Learning, 52(3), 239-281. doi: 10.1023/a:1024068626366
# 第256页，分类数据（2）
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/5/30

two_normals_classification.DataGenerator <- function(dataConf){
  require(MASS)
  n0 <- dataConf$n0
  n1 <- dataConf$n1
  mu0 <- dataConf$mu0
  mu1 <- dataConf$mu1
  sigma0 <- dataConf$sigma0
  sigma1 <- dataConf$sigma1  
  x0<-mvrnorm(n0,mu0,sigma0)
  x1<-mvrnorm(n1,mu1,sigma1)
  x<-rbind(x0,x1)
  y<-c(rep(0,n0),rep(1,n1))  
  n <- n0 + n1
  shuffle <- sample(x= 1:n, size = n, replace = FALSE)
  x <- x[shuffle, ]
  y <- factor(y[shuffle])
  data <- data.frame(x, y)
  return(data)
}


two_normals_classification.Prepackages <- c("MASS")


two_normals_classification.validate.n <- function(name, value) {
  if(!is.double(value) || length(value) > 1)  {
    warning(paste("param", name, "is not a integer"))
    return(FALSE)
  }
  if(value %% 1 != 0) {
    warning(paste("param", name , "is not a integer"))
    return(FALSE)
  }
  if(value < 1) {
    warning(paste("sample count:", name, ">= 1"))
    return(FALSE)
  }  
  return(TRUE)
}

two_normals_classification.validate.mu <- function( name, value) {
  if( !is.vector(value) ) {
     return(FALSE)
  }
  if( !is.numeric(value) ) {
    return(FALSE)
  }
  return(TRUE)
}

two_normals_classification.validate.cov <- function(name, value) {
  
  if( !is.matrix(value) ) {
    return(FALSE)
  }
  if( !is.numeric(value)) {
    return(FALSE)
  }
  if( nrow(value) != ncol(value)) {
    
    return(FALSE)
  }
  if( !isSymmetric.matrix(value)) {
    return(FALSE)
  }
  return(TRUE)
}

two_normals_classification.conf.list <- list(
  list(name="n0", tip= "count of y=0", type="", valid_fun = two_normals_classification.validate.n),
  list(name="n1", tip = "count of y=1", type="", valid_fun = two_normals_classification.validate.n),
  list(name="mu0", tip = "mean of first class", type = "", valid_fun = two_normals_classification.validate.mu),
  list(name="mu1", tip = "mean of class 1", type = "", valid_fun = two_normals_classification.validate.mu),
  list(name="sigma0", tip = "cov of class 0", type = "", valid_fun = two_normals_classification.validate.cov),
  list(name="sigma1", tip = "cov of class 1", type = "", valid_fun = two_normals_classification.validate.cov)
  )

two_normals_classification.conf.shortcuts <- list(
	bengio_infer_ml_sim1 = list(n0 = 100, n1=100, mu0 = rep(0,2), mu1 = rep(1,2), sigma0 = diag(2), sigma1 = (1/2)*diag(2)),
    bengio_infer_ml_sim2 = list(n0 = 100, n1=100, mu0 = rep(0,2), mu1 = rep(1,2), sigma0 = diag(2), sigma1 = (1/6)*diag(2)),
	bengio_infer_ml_sim3 = list(n0 = 1000, n1=1000, mu0 = rep(0,2), mu1 = rep(1,2), sigma0 = diag(2), sigma1 = (1/2)*diag(2)),
    bengio_infer_ml_sim4 = list(n0 = 1000,n1 = 1000, mu0 = rep(0,2), mu1 = rep(1,2), sigma0 = diag(2), sigma1 = 0.173*diag(2))
)


two_normals_classification.validation <- function(dataConf) {
  valid <- ValidateConfiguration( conf_impl = dataConf, 
                              conf_def = two_normals_classification.conf.list, 
                              shortcut_list =  two_normals_classification.conf.shortcuts  )
  return(valid)
}