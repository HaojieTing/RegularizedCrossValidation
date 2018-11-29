# 平方损失
#
# 平方损失主要用于回归任务。
# 为预测值与真实值之间差距的平方。
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/7/30

squared_loss.Generator <- function(metric.conf) {
  # 计算平方损失值及其均值.
  #
  # Args:
  #   指标配置: list类型
  #       gold 字段存储响应值的真实值。
  #       pre 字段存储响应值的预测值。
  # Returns:
  #   list类型
  #       第一个元素为泛化误差估计值
  #       第二个元素为损失向量。
  response.gold <- metric.conf$resp.gold
  response.pre  <- metric.conf$resp.pre
  # 检查变量的类型。
  if (!is.numeric(response.gold)&&!is.numeric(response.pre)) {
    stop("In one-zero loss, resp or gold is not a valid type")
  }
  loss.vector <- (response.pre - response.gold)^2 
  ge.est <- mean(loss.vector)
  return(list(est = ge.est, loss.vector))
}

squared_loss.Prepackages <- c()

squared_loss.validation <- function(metric.conf) {
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

