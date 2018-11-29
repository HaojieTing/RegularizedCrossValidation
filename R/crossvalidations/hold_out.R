# hold-out验证
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: xxxx/xx/xx
# 修改记录:
#     wrb@2017/11/8: 修改了交叉验证切分集的存储方式

hold_out.Generator <- function(cvConf) {
  if (is.null(cvConf$data)) stop("Please specify cv$data.")
  n <- nrow(cvConf$data)
  n1 <- cvConf$n1
  prop <- cvConf$prop
  if(is.null(n1)) {
    if(!is.null(prop)) {
      n1 <- round(prop * n)
    }
  }
  train_index <- sample(x = 1:n, size = n1, replace = FALSE)  
  split <- vector("list", 2)  
  split[[1]] <- train_index
  split[[2]] <- (1:n)[-train_index]
  holdout <- list()
  holdout[["partitions"]] <- split
  return(holdout)
}


hold_out.Prepackages <- c()


hold_out.validation <- function(cvConf) {
  if( is.null(cvConf$n1) && is.null(cvConf$prop)) return(FALSE)
  return(TRUE)
}