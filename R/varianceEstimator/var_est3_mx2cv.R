#******************************************************************************\
# 作者信息:
#   王瑞波  wangruibo@sxu.edu.cn 山西大学 语义网研究室
#
# CopyRight (c) 2012-2014, ALL RIGHTS RESERVED.
#
# 模块名:
#   mx2交叉验证的第三种方差估计。
#   
# 简介:
#   原来的第三个方差估计，现在论文的第二个方差估计。
#
# 创建日期：
#   2015/2/7
#
# 修改历史:
#   |修改人       |    邮件               | 修改时间   |  修改历史
# ----------------------------------------------------------------------------
#
# TODOs:
#
#*****************************************************************************/

var_est3_mx2cv.EstForOneExpr <- function(mu, veConf) {
  # 对于一个实验的第三种方差估计;
  #
  # Args:
  #   mu: 一个实验对应的mu;
  #   m:  mx2cv中的m
  #
  # Return:
  #   第三种方差估计值
  m         <- veConf$m
  if(length(mu) != 2 *m +1)
    stop(paste("给出的mu的长度不对：",length(mu), "m:", m))
  minu_one <- FALSE
  if( !is.null(veConf$minu_one) ) minu_one <- veConf$minu_one 
  mu_all <- mu[2*m+1]
  est = 0
  for(i in 1:(2*m)){
    est = est +(mu[i]-mu_all) ** 2
  }
  if( !minu_one )  est = (1/(2*m)) * est
  else   est = (1/(2*m -1)) * est
  return(est)  
}


var_est3_mx2cv.getfreedegree <- function(m) {
  return(2*m-1)
}

var_est3_mx2cv.Estimator <- function(veConf) {
  # mx2cv的第二种方差估计.
  #
  # Args: veConf: 方差估计的配置;
  #   veConf$rpt: 数据的模拟次数;
  #
  # Return: list
  #   所有的方差估计;
  data_file <- veConf$data_file
  m         <- veConf$m
  if( !file.exists(data_file) ) {
    stop("the given data file does not exist.")
  }
  load( data_file )  # load muv.
  muv_est <- rep(NA, nrow(muv))
  for(i in 1:length(muv_est)){
    muv_est[i] <- var_est3_mx2cv.EstForOneExpr(muv[i,], m, veConf)
  }
  if(!is.null(conf_TASK_VAR_EST$multi) && conf_TASK_VAR_EST$multi == TRUE) {
    muv_est <- matrix(NA, nrow(muv), m-2+1)
    m_index <- 2
    while(m_index <= m) {
      tmuv <- muv[, 1:(2*m_index)]
      tmuv <- cbind(tmuv, rowMeans(tmuv))
      for(i in 1:nrow(tmuv)){
        muv_est[i,m_index-1] <- var_est3_mx2cv.EstForOneExpr(tmuv[i,], m_index, veConf)
      }        
    }
  }
  return(muv_est)
}


var_est3_mx2cv.validation <- function(veConf) {
  # 方差估计二的参数的验证;
  #
  # Args:
  #   veConf$m: mx2中的m;
  # Return:
  #   TRUE: 正确配置; FALSE:错误的配置;
  if(is.null(veConf$m)) return(FALSE)
  if( is.null(veConf$minu_one) ) {
    warning("You should specify the 'minu_one config for var est 3")    
  }
  return(TRUE)
}
