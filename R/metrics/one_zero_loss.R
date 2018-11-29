# 0-1损失
#
# 0-1损失常用于分类任务。输入一个样本集的响应变量的真实取值
# 以及预测值，计算出0-1损失向量，及其均值。
#
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/7/4


one_zero_loss.Generator <- function (metric.conf) {
  # 计算0-1损失值及其均值。
  #
  # Args:
  #   指标配置: list类型
  #       gold字段存储响应变量的真值.
  #       pre字段存储响应变量的预测值
  # Returns:
  #   list类型:
  #       第一个元素为泛化误差估计值.
  #       第二个元素为损失向量
  response.gold <- metric.conf$resp.gold
  response.pre  <- metric.conf$resp.pre
  # 检查变量的类型。
  if (!is.factor(response.gold)&&!is.character(response.gold)) {
    stop("In one-zero loss, resp gold is not a valid type")
  }
  if (!is.factor(response.pre) && !is.character(response.pre)) {
    stop("In one-zero loss, resp pre is not a valid type")
  }
  loss.vector <- abs( response.pre != response.gold) 
  ge.est <- mean(loss.vector)
  return(list(est = ge.est, loss.vector))
}

one_zero_loss.Prepackages <- c()

one_zero_loss.validation <- function(metric.conf) {
  # 验证指标配置文件的正确性.
  # Args:
  #   指标配置文件.
  # Returns:
  #   指标配置文件是否正确。
  if(is.null(metric.conf$gold)||is.null(metric.conf$pre)) {
    return(FALSE)
  }
  return(TRUE)
}