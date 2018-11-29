# 协方差函数值及其参数值模拟程序
#
# 协方差模拟任务：
#    协方差模拟任务是指：给定数据大小以及训练集大小，从总体
#    中产生出多个数据集，并给定两个有一定重叠的切分，模拟数
#    据在这两个切分上的协方差值。
#
# 配置项:
#     1. rpt: 模拟次数;
#     2. overlapCount: 两个切分的训练集观测重叠个数;
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/5/9

ValidateCovarianceSimulationConfiguration <- function(task.conf) {
  # 校验协方差模拟的配置项。
  #
  # Args:
  #   task.conf: 任务配置项
  #
  # Returns:
  #   TRUE: 配置正确，否则程序终止。
  if (is.null(task.conf$overlapCount)) {
    stop("Need to specify 'overlapCount' for conf_TASK_COV_SIMU")  
  }
  if (is.null(task.conf$rpt)) {
    stop("Need to specify 'rpt' for conf_TASK_COV_SIMU")
  }
  return(TRUE)
}

ValidateCovarianceSimulationConfiguration(task.conf)
#附加约束
if( "hold_out" != crossvalidation.conf$name) {
  stop("Cov simulation only support hold-out validation")
}

task.conf.overlapCount <- task.conf$overlapCount
task.conf.rpt <- task.conf$rpt
task.conf.n1 <- crossvalidation.conf$n1
task.conf.n <- crossvalidation.conf$n
source("covariance_function_simulation.R", encoding="UTF-8")
source("result_print_out.R", encoding="UTF-8")