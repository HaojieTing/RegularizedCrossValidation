# 常用函数集合。
# 
# 对于这些常用函数，可能会在很多任务模块中使用。
# 建议，在任务模块开始时，加载本文件。
# 
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: xxxx/xx/xx
# 修改记录：
#    wrb@2017/11/10: 加入了数据集、算法等公用模块的验证函数。

WorkerInit <-function(packages){
  for(p in packages){
    library(p, character.only=TRUE)
  }
  NULL
}

# ------------数据集、算法等公共模块的验证函数------------

ValidateAndResolveDataSetConfiguration <- function(dataset.conf) {
  # 校验数据集配置是否正确。
  #
  # 校验数据集的配置是否正确：
  #    - 如果正确，则返回解析掉快捷项后的配置。
  #    - 如果不正确，则直接报错。
  #
  # Args:
  #   dataset.conf: 数据集的配置项。具体参照数据集配置说明。
  #
  # Returns:
  #   解析掉快捷方式后的数据集配置。
  if(ValidateDataSetConfiguration(dataset.conf)) {
    dataset.conf <- ResolveShortcutInDataConf(dataset.conf)
    return(dataset.conf)
  } else {
    stop(paste("Failed to validate configuration of data set!", dataset.conf["name"], sep=":"))
  }
}


ValidateAndResolveAlgorithmConfiguration <- function(algorithm.conf) {
  # 校验机器学习算法配置项是否正确。
  #
  # 校验算法配置是否正确。
  #     - 如果正确，则返回解析掉快捷项后的配置。
  #     - 如果不正确，则直接报错。
  #
  # Args:
  #   algorithm.conf: 算法的配置项。具体参照机器学习算法的配置说明。
  #
  # Returns:
  #    解析掉快捷方式后的算法配置。
  if (ValidateAlgorithmConfiguration(algorithm.conf) ) {
    algorithm.conf <- ResolveShortCutInAlgorithmConfiguration(algorithm.conf)
  } else {
    stop("Failed to validate algorithm configuration[ALGOR]")
  }
}

ValidateAndResolveCrossValidationConfiguration <- function(crossvalidation.conf) {
  # 校验交叉验证方法配置项是否正确。
  # 
  # 校验交叉验证方法的配置项。
  #     - 如果正确，则返回解析掉快捷项后的配置。
  #     - 如果不正确，则直接报错。
  #
  # Args:
  #   crossvalidation.conf:交叉验证的配置项。具体参照交叉验证的配置说明。
  #
  # Returns:
  #   解析掉快捷方式后的交叉验证配置。
  if (ValidateCrossValidationConfiguration(crossvalidation.conf)) {
    crossvalidation.conf <- ResolveShortCutInCrossValidationConf(crossvalidation.conf)
  } else {
    stop("Failed to validate configuration of cross validation!")
  }
}



ResolveConfigurationShortcut <- function(conf.impl, shortcut.list) {
  # 解析配置中的快捷配置。
  #
  # Args:
  #   conf.impl: 具体的一个配置信息，为list.
  #   shorcut.list: 快捷配置列表.
  #
  # Returns:
  #   解析掉快捷配置后的配置信息
  if( !is.null(conf.impl$shortcut_name) ) {
    shortcut.name <- conf.impl$shortcut_name
    if( shortcut.name %in% names(shortcut.list) ) {
      conf.impl.temp <- shortcut.list[[shortcut.name]]
      for( name in names(conf.impl)) {
        if ("shortcut_name" == name) {          
          next
        }
        conf.impl.temp[[name]] <- conf.impl[[name]]
      }
      conf.impl <- conf.impl.temp
    } else {
      conf.impl <- NA
    }
  }
  return(conf.impl)
}


ValidateConfiguration <- function(conf_impl, shortcut_list, conf_def) {
  # 
  #
  #
  #
  #
  conf_impl <- ResolveConfigurationShortcut(conf_impl, shortcut_list)
  valid <- TRUE
  for(i in 1:length(conf_def)) {
    entry <- conf_def[[i]]
    if(is.null(conf_impl[[entry$name]])) {
      valid <- FALSE 
    } else {
    valid <- entry$valid_fun(entry$name, conf_impl[[entry$name]]) && valid
    }
  }  
  return(valid)
}

CheckVariableExist <- function(variable.name, error.message=NA, error.stop=TRUE) {
  # 检查给定变量名的变量在当前运行环境中是否存在。
  #
  # Args:
  #   variable.name: 要检查的变量名.
  #   error.message: 如果不存在，要输出的错误信息，如果不提供，则输出标准错误信息.
  #   error.stop: 如果不存在，是否停止程序运行.
  # Returns:
  #   TRUE/FALSE 变量存在为TRUE, 否则为FALSE.
  error.default.message <- paste("Variable", variable.name, "not exists!", sep=" ")
  if(!is.na(error.message)) {
    error.default.message <- error.message
  }
  if( !exists(variable.name) ) { #如果不存在变量名所对应的变量.
    if(error.stop) {
      stop(error.default.message)
    } else {
      warning(error.default.message)
    }
    return(FALSE)
  }
  return(TRUE)
}

convertParitionSetToPartitionMatrix <- function (partition.set) {
  # 将切分集合转化为切分矩阵.
  # 
  # 切分矩阵为0-1矩阵，行为样本数，列为切分次数.
  # 切分矩阵中，若干第i行第j列函数为1，则表明第i
  # 个观测出现在第j个切分中
  #
  # Args:
  #   partition.set: 切分集合，含多个切分， 为list对象。
  #
  # Returns:
  #   partition.matrix: 切分矩阵.
  partition.set.size <- length(partition.set)
  if(partition.set.size <= 1) {
    stop("partition set size should not less than 2.")
  }
  partition.first <- partition.set[[1]]
  row.size <- length(union(partition.first[[1]], partition.first[[2]]))
  column.size <- partition.set.size
  partition.matrix <- matrix(rep(0, row.size * column.size), row.size)
  for(i in 1:partition.set.size) {
    partition.i <- partition.set[[i]]
    partition.i.trainingset <- partition.i[[1]]
    partition.matrix[partition.i.trainingset, i] <- 1
  }
  return(partition.matrix)
}

overlappingCountsOfPartitionSet <- function(partition.matrix) {
  # 基于切分矩阵，计算重叠个数矩阵。
  #
  # Args: 
  #   partition.matrix: 切分矩阵，为0-1矩阵。
  # Returns:
  #   重叠个数矩阵, 为JxJ矩阵，J为切分集中切分重复次数
  #   ，为partition.matrix的列数.
  return(t(partition.matrix)%*%partition.matrix)
}

occurrencesCountOfPartitionSet <- function(partition.matrix) {
  # 基于切分矩阵，计算每个样本在所有的训练集中的出现次数
  #
  # Args:
  #   partition.matrix: 切分矩阵，为0-1矩阵
  # Returns:
  #   出现次数向量，为n维向量，n为观测个数.
  column.size <- ncol(partition.matrix)
  unit.vector <- matrix(rep(1, column.size),column.size)
  return(c(partition.matrix %*%unit.vector))
}

# ---------------- 验证、配置运行环境函数，及包下载函数 --------------------------------
# 参考URL: http://blog.csdn.net/liu365560704/article/details/70321153

downloadAllNeededPackages <- function(packages, dirpath) {
  # 下载给定packages及所有的依赖包，并存储到dirpath给定的目录中。
  #
  # Args:
  #   packages: 要下载的R包。
  #   dirpath:  要存储到的目录。
  # Return:
  #   NULL
  packs <- packages
  packages <- unlist(  
    tools::package_dependencies(packs, available.packages(),which=c("Depends", "Imports"), recursive=TRUE)  
  )  
  packages <- union(packs, packages)  
  download.packages(packages, destdir=dirpath, type="source")  
}  

installAllNeededPackages <- function(packages, dirpath) {
  # 安装本地的离线包，所有的离线包存储在dirpath目录中。
  #
  # Args:
  #   packages: 要安装的R包。
  #   dirpath: 存储所安装的R包的目录。
  library(tools)  
  path <- dirpath  
  write_PACKAGES(path, type="source")  
  myPackages <- packages
  install.packages(myPackages, contriburl=paste("file:",path,sep=''),type="source")   
}