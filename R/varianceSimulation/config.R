# 方差模拟模块配置
#
# 检查方差模拟模块的配置是否正确，
# 并调用方差模拟模块计算方差值。
#
# 配置项:
#   dataRepCount: 数据重复的次数.
#   partitionSetRepCount: 切分集合重复的次数.
#   partitionSetRndSeed: [可选]是否启用切分集产生的随机数种子.
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/5/21

validationVarianceSimulationConfiguration <- function(task.conf) {
  # 校验方差模拟任务的配置项。
  #
  # Args:
  #   task.conf: 方差模拟任务配置项
  #
  # Returns:
  #   TRUE: 配置正确，否则程序终止。
  if (is.null(task.conf$dataRepCount)) {
    stop("Need to specify dataRepCount for variance simulation task.")
  }
  if (is.null(task.conf$partitionSetRepCount)) {
    stop("Need to specify partitionSetRepCount for variance simulation task.")
  }
  if (is.null(task.conf$partitionSetRndSeed)) {
    warning("partitionSetRndSeed is not specified, default value is TRUE.")
  }
  return(TRUE)
}

validationVarianceSimulationConfiguration(task.conf)

varsim.conf.dataRepCount <- task.conf$dataRepCount
varsim.conf.partitionSetRepCount <- task.conf$partitionSetRepCount
varsim.conf.partitionSetRndSeed <- TRUE
if (!is.null(task.conf$partitionSetRndSeed)) {
  varsim.conf.partitionSetRndSeed <- task.conf$partitionSetRndSeed
}
source("variance_simulation.R", encoding ="UTF-8")
source("write_result_out.R", encoding = "UTF-8")
setwd("../")
