LoadAlgorithmGenerator <- function(algorithm.conf){
  # 加载机器学习算法的生成函数和附属包
  #
  # Args:
  #   algorithm.conf: 机器学习算法配置.
  #
  # Returns:
  #   1. 机器学习算法的生成函数.
  #   2. 机器学习算法的附属包.
  algorithm.name <- algorithm.conf$name
  algorithm.type    <- algorithm.conf$type  
  algorithm.src <- algorithm.conf$src
  if( is.null(algorithm.src) ) {
    if ( ("classification" != algorithm.type) && ("regression" != algorithm.type) ) {
      stop("the algorithm's type is not supported.")
    }
    path = getwd()
    if("R" %in% list.files(path) && "LICENSE" %in% list.files(path))
      path <- file.path(path, "R", fsep = .Platform$file.sep)
    if ('mlAlgorithms' %in% list.files(path)) {
      path <- file.path(path,'mlAlgorithms', algorithm.type, paste(algorithm.name,'.R',sep=""), 
                        fsep=.Platform$file.sep)
    } else {
      path = file.path(path, '..','mlAlgorithms', algorithm.type, paste(algorithm.name,'.R',sep=""), 
                       fsep=.Platform$file.sep)
    }
    if ( file.exists(path) ) {
      source(path, encoding = "UTF8")
      algor.generator <- get(paste(algorithm.name, 'TrainAndTest', sep="."))
      algor.packages <- NULL
      if ( exists(paste(algorithm.name, 'Prepackages', sep='.')) ) {
        algor.packages <- get(paste(algorithm.name, 'Prepackages', sep='.'))
      }
      return( list(algor.generator, algor.packages) )
    } else {
      stop("ml algor is not found.")
    }     
  } else if("WEKA" == algorithm.src) {
    path = getwd()
    if ('mlAlgorithms' %in% list.files(path)) {
      path <- file.path(path,'mlAlgorithms', 'general', 'WEKAClassifiers.R', fsep=.Platform$file.sep)
    } else {
      path = file.path(path, '..','mlAlgorithms', 'general', 'WEKAClassifiers.R', fsep=.Platform$file.sep)
    }
    source(path, encoding="UTF-8")
    WEKAClassifiers.replaceVarNames(algorithm.name)
    algor.generator <- get(paste(algorithm.name, 'TrainAndTest', sep="."))
    algor.packages <- NULL
    if (exists(paste(algorithm.name, 'Prepackages', sep='.'))) {
      algor.packages <- get(paste(algorithm.name, 'Prepackages', sep='.'))
    }
    return( list(algor.generator, algor.packages) )
  } else {
    stop("donot support current algor source")
  }
}


ValidateAlgorithmConfiguration <- function(algorithm.conf) {
  # 校验机器学习算法的配置信息.
  #
  # Args:
  #   algorithm.conf: 机器学习算法的配置信息;
  #
  # Returns:
  #   TRUE/FALSE: 配置信息是否正确;
  return.value <- FALSE
  if (is.null(algorithm.conf$name) || is.null(algorithm.conf$type)) {
    warning("Need to specify the name and type property for algorithm[ALGOR]")
    return(FALSE)
  }
  LoadAlgorithmGenerator(algorithm.conf)
  if (exists(paste(algorithm.conf$name, 'validation', sep='.'))) {
    validate_function <- get(paste(algorithm.conf$name, 'validation', sep='.'))
    return.value <- validate_function(algorithm.conf)
  } else {
    return.value <- TRUE
  }
  return(return.value)
}


ResolveShortCutInAlgorithmConfiguration <- function(algorithm.conf) {
  # 解析机器学习算法配置中的快捷配置.
  #
  # Args:
  #   algorithm.conf: 机器学习算法配置。
  # 
  # Returns:
  #   解析掉快捷配置后的机器学习算法配置。
  LoadAlgorithmGenerator(algorithm.conf)
  if( exists(paste(algorithm.conf$name, "conf", "shortcuts", sep=".")) ) {
    shortcuts <- get(paste(algorithm.conf$name, "conf", "shortcuts", sep="."))
    algorithm.conf <- ResolveConfigurationShortcut(algorithm.conf, shortcuts)
  }
  return(algorithm.conf)
}