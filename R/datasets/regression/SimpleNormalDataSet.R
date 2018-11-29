# 数据来自:
#     Nadeau, C., & Bengio, Y. (2003). Inference for the Generalization Error. Machine Learning, 52(3), 239-281. doi: 10.1023/a:1024068626366
# 第256页。
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/5/30

SimpleNormalDataSet.DataGenerator <- function(dataConf){
  require("MASS")
  
  n <- dataConf$n
  mu0  <- dataConf$mu_x
  sigmax <- dataConf$sigma_x
  beta0  <- dataConf$mu_bias_y
  beta1 <- dataConf$mu_scale_y
  sigmay <- dataConf$sigma_y
  if( length(mu0) != 1) {
    warning("SimpleNormalDataSet only support one-dimensional x")
  }
  y<-rep(NA,n)
  x<-mvrnorm(n,mu0,sigmax)
  for(i in 1:n)
  {
    y[i] <- mvrnorm(1,beta0+beta1*x[i],sigmay)
  }
  return(as.data.frame(cbind(x, y)))
}


SimpleNormalDataSet.Prepackages <- c()


SimpleNormalDataSet.validate.n <- function(name, value) {
  if(!is.double(value) || length(value) > 1)  {
       warning(paste("param", name, "is not a integer:SimpleNormalDataSet[DATASET]"))
       return(FALSE)
  }
  if(value %% 1 != 0) {
      warning(paste("param", name , "is not a integer:SimpleNormalDataSet1[DATASET]"))
      return(FALSE)
  }
  if(value < 1) {
      warning(paste("sample count", name, " >= 1:SimpleNormalDataSet[DATASET]"))
      return(FALSE)
  }  
  return(TRUE)
}


SimpleNormalDataSet.validate.mu_and_sigma <- function(name, value) {
  if(1  != length(value) || !is.numeric(value) ) {
    return(FALSE)
  }
  return(TRUE)
}


SimpleNormalDataSet.conf.list <- list(
    list(name = "n",    tip = "", type="", valid_fun = SimpleNormalDataSet.validate.n),
    list(name = "mu_x", tip = "", type="", valid_fun = SimpleNormalDataSet.validate.mu_and_sigma ),
    list(name = "sigma_x", tip = "", type="", valid_fun = SimpleNormalDataSet.validate.mu_and_sigma ),
    list(name = "mu_bias_y", tip = "", type="", valid_fun = SimpleNormalDataSet.validate.mu_and_sigma),
    list(name = "mu_scale_y", tip = "", type="", valid_fun = SimpleNormalDataSet.validate.mu_and_sigma),
    list(name = "sigma_y", tip = "", type="", valid_fun=SimpleNormalDataSet.validate.mu_and_sigma)
  )


SimpleNormalDataSet.conf.shortcuts <- list(
     infor_for_gene_err = list(n = 2000, mu_x = 10.0, sigma_x = 1.0, 
			mu_bias_y = 100, mu_scale_y = 0.1, sigma_y =  9.97, source="infor_for_gene_err"),
	 infor_for_gene_err_sim1 = list(n = 200, mu_x = 10.0, sigma_x = 1,
			mu_bias_y = 100, mu_scale_y = 1, sigma_y = 97, source="infor_for_gene_err"),
	 infor_for_gene_err_sim2 = list(n=200, mu_x = 10.0, sigma_x = 2,
	        mu_bias_y = 100, mu_scale_y = 2, sigma_y =64, source = "infor_for_gene_err"),
	 infor_for_gene_err_sim4 = list(n=2000, mu_x = 10, sigma_x=5, 
	        mu_bias_y = 100, mu_scale_y = 0.1, sigma_y = 9, source= "infor_for_gene_err")
  )


SimpleNormalDataSet.validation <- function(dataConf) {
  valid <- ValidateConfiguration( conf_impl = dataConf, 
                     conf_def = SimpleNormalDataSet.conf.list, 
                     shortcut_list =  SimpleNormalDataSet.conf.shortcuts)
  return(valid)
}

