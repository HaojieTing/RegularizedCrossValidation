# 准确率、召回率和F值
#
# 在分类任务中，根据混淆矩阵，计算
# 类别的准确率、召回率和F值三个性能指标。
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/7/31

prf.Generator <- function(metric.conf) {
  # 计算准确率，召回率和F值.
  #
  # Args:
  #   指标配置: list类型
  #       gold 字段存储响应值的真实值。
  #       pre 字段存储响应值的预测值。
  # Returns:
  #   list类型
  #       P: 准确率
  #       R: 召回率
  #       F: F-值(alpha=1)
  response.gold <- metric.conf$resp.gold
  response.pre  <- metric.conf$resp.pre
  response.class <- metric.conf$class
  gold.bool <- (response.gold == response.class)
  pre.bool  <- (response.pre  == response.class)
  correct.bool <- (gold.bool & pre.bool)
  precision <- 0
  recall <- 0
  f <- 0
  if( sum(pre.bool) != 0 )
     precision <- sum(correct.bool) / sum(pre.bool)
  if(sum(gold.bool) != 0)
     recall <-    sum(correct.bool) / sum(gold.bool)
  if(precision != 0 && recall != 0)
    f <- 2 * precision * recall / (precision + recall)
  return(list(
    P = precision,
    R = recall,
    F = f
  ))
}

prf.Prepackages <- c()


prf.validation <- function(metric.conf) {
  # 验证指标配置文件的正确性.
  # Args:
  #   指标配置文件.
  #    gold: 字段存储响应值的真实值。
  #    pre: 字段存储响应值的预测值。
  #    class: 要计算指标的类别。
  # Returns:
  #   指标配置文件是否正确。
  if(is.null(metric.conf$gold)||is.null(metric.conf$pre)) {
    return(FALSE)
  }
  if(is.null(metric.conf$class)) {
    return(FALSE)
  }
  return(TRUE)
}