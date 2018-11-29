# 评价指标的加载模块。
#
# 主要提供两个功能:
# - 验证评价指标的配置文件是否正确；
# - 根据评价指标的配置文件来加载特定的配置
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/7/4

LoadPerformanceMetricGenerator <- function (metric.conf) {
  # 加载特定的模型评价指标。
  #
  # Args:
  #   metric.conf: 模型评价指标的配置
  # Returns:
  #   1. 评价指标的生成函数;
  #   2. 评价指标所需的附属包
  if(is.null(metric.conf)) return(NULL)
  metric.name <- metric.conf$name
  path <- getwd()
  if('metrics' %in% list.files(path) ) {
    path <- file.path(path, 'metrics', paste(metric.name, ".R", sep = ""),
                      fsep=.Platform$file.sep)
  } else {
    path <- file.path(path, '..', 'metrics', paste(metric.name, ".R", sep = ""),
                      fsep=.Platform$file.sep)
  }
  if (file.exists(path)) {
    source(path, encoding = "UTF8")
    metric.generator <- get(paste(metric.name, "Generator", sep="."))
    metric.packages <- NULL
    if ( exists(paste(metric.name, "Prepackages", sep=".")) ) {
      metric.packages <- get(paste(metric.name, "Prepackages", sep="."))
    }
    return(list(metric.generator, metric.packages))
  } else {
    stop("cannot get the given performance metric.")
  }
}

ValidatePerformanceMetricConfiguration <- function(metric.conf) {
  # 验证模型评价指标配置是否正确.
  # Args:
  #   metric.conf: 模型评价指标配置文件
  # Returns:
  #    TRUE: 配置正确; FALSE: 配置错误
  return.value <- TRUE
  if (is.null(metric.conf$name)) {
    warning("The name field in configuration of performance metric must be specified.")
    return(FALSE)
  }
  LoadCrossValidationGenerator(metric.conf$name)
  if (exists(paste(metric.conf$name, "validation", sep="."))) {
    cv.validate <- get(paste(metric.conf$name, "validation", sep="."))
    return.value <- cv.validate(metric.conf)
  } else {
    return.value <- TRUE
  }
  return(return.value)
}