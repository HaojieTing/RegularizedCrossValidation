LoadCrossValidationGenerator <- function(crossvalidation.name) {
  # 加载交叉验证方法的生成函数和附属包
  #
  # Args:
  #   crossvalidation.name: 交叉验证方法的名称.
  #
  # Returns:
  #   1. 交叉验证方法的生成函数.
  #   2. 交叉验证方法的附属包.
  path = getwd()
  if("R" %in% list.files(path) && "LICENSE" %in% list.files(path))
    path <- file.path(path, "R", fsep = .Platform$file.sep)
  if('crossvalidations' %in% list.files(path) ) {
    path <- file.path(path, 'crossvalidations', paste(crossvalidation.name, ".R", sep = ""),
                     fsep=.Platform$file.sep)
  } else {
    path <- file.path(path, '..', 'crossvalidations', paste(crossvalidation.name, ".R", sep = ""),
                     fsep=.Platform$file.sep)
  }
  if ( file.exists(path) ) {
    source(path, encoding = "UTF8")
    cv.generator <- get(paste(crossvalidation.name, "Generator", sep="."))
    cv.packages <- NULL
    if ( exists(paste(crossvalidation.name, "Prepackages", sep=".")) ) {
      cv.packages <- get(paste(crossvalidation.name, "Prepackages", sep="."))
    }
    return(list(cv.generator, cv.packages))
  } else {
    stop("cannot get the given cross validation schema.")
  }
}


ValidateCrossValidationConfiguration <- function(crossvalidation.conf) {
  # 验证交叉验证配置信息是否正确.
  #
  # Args:
  #   crossvalidation.conf: 交叉验证的配置信息.
  # 
  # Returns:
  #   TRUE/FALSE: 配置信息是否正确.
  return.value <- TRUE
  if (is.null(crossvalidation.conf$name)) {
    warning("The name field in configuration of cross validation must be specified.")
    return(FALSE)
  }
  LoadCrossValidationGenerator(crossvalidation.conf$name)
  if (exists(paste(crossvalidation.conf$name, "validation", sep="."))) {
    cv.validate <- get(paste(crossvalidation.conf$name, "validation", sep="."))
    return.value <- cv.validate(crossvalidation.conf)
  } else {
    return.value <- TRUE
  }
  return(return.value)
}


ResolveShortCutInCrossValidationConf <- function(crossvalidation.conf) {
  # 解析交叉验证配置中的快捷配置
  #
  # Args:
  #   crossvalidation.conf: 交叉验证配置信息;
  #
  # Returns:
  #   解析掉快捷配置后的交叉验证配置信息；
  LoadCrossValidationGenerator(crossvalidation.conf$name)
  if( exists(paste(crossvalidation.conf$name, "conf", "shortcuts", sep="."))) {
    cv.shortcuts <- get(paste(crossvalidation.conf$name, "conf", "shortcuts", sep="."))
    crossvalidation.conf <- ResolveConfigurationShortcut(crossvalidation.conf, cv.shortcuts)
  }
  return(crossvalidation.conf)
}