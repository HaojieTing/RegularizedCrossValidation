LoadDataSetGenerator <-  function(dataset.conf){
  # 根据数据集配置，加载数据集的产生函数及附属包.
  #
  # Args:
  #   dataset.conf: 数据集配置.
  #
  # Returns:
  #    1. 数据集的生成函数;
  #    2. 生成数据集所用的R包;
  data.name <- dataset.conf$name
  data.type <- dataset.conf$type
  data.source <- dataset.conf$src
  path <- getwd()  
  if("R" %in% list.files(path) && "LICENSE" %in% list.files(path))
    path <- file.path(path, "R", fsep = .Platform$file.sep)
  if ( is.null(data.source) || "" == data.source ) {
    if('datasets' %in% list.files(path)) {
      path <- file.path(path,'datasets', data.type, paste(data.name,'.R',sep=""), 
                        fsep=.Platform$file.sep)
    } else {
      path <- file.path(path, '..','datasets', data.type, paste(data.name,'.R',sep=""), 
                       fsep=.Platform$file.sep)
    }  
    if(file.exists(path)){
      source(path, encoding="UTF-8")
      data.generator <- get(paste(data.name, 'DataGenerator', sep="."))
      data.packages <- NULL
      if ( exists(paste(data.name, 'Prepackages', sep='.')) ) {
        data.packages <- get(paste(data.name, 'Prepackages', sep='.'))
      }
      return( list(data.generator, data.packages))
    } 
 } else if("RMLD" == data.source) {
    if(data.name %in% uci_cla_datanames && data.type == 'classification') {
      path = getwd()
      if('datasets' %in% list.files(path)) {
        path <- file.path(path,'datasets', 'classification', 'uci_classification.R', 
                          fsep=.Platform$file.sep)
      } else {
        path <- file.path(path, '..','datasets', 'classification', 'uci_classification.R',  
                          fsep=.Platform$file.sep)
      }      
      source(path, encoding="UTF-8")
      uci_classification.replaceVarNames(data.name)      
      data.generator <- get(paste(data.name, 'DataGenerator', sep="."))
      data.packages <- NULL
      if ( exists(paste(data.name, 'Prepackages', sep='.')) ) {
        data.packages <- get(paste(data.name, 'Prepackages', sep='.'))
      }      
      return( list(data.generator, data.packages) )
    }
    else stop("fail to load")
  } else if ("WEKA" == data.source){
    path = getwd()
    if('datasets' %in% list.files(path)) {
      path <- file.path(path,'datasets', 'general', 'weka_dataset_loader.R', 
                        fsep=.Platform$file.sep)
    } else {
      path <- file.path(path, '..','datasets', 'general', 'weka_dataset_loader.R',  
                        fsep=.Platform$file.sep)
    }      
    source(path, encoding="UTF-8")
    weka_dataset_loader.replaceVarNames(data.name)      
    data.generator <- get(paste(data.name, 'DataGenerator', sep="."))
    data.packages <- NULL
    if ( exists(paste(data.name, 'Prepackages', sep='.')) ) {
      data.packages <- get(paste(data.name, 'Prepackages', sep='.'))
    }      
    return( list(data.generator, data.packages) )
  } 
}


ValidateDataSetConfiguration  <- function(data.conf) {
  # 数据集配置文件校验函数.
  #
  # 校验数据集是否存在，相应的配置是否正确。
  # 首先校正数据集配置中是否有type和name的配置，
  # 进而校验余下的配置是否和具体的数据集配置吻合。
  #
  # Args:
  #   data.conf: list对象，存储数据集配置。
  #   
  # Returns:
  #   TRUE/FALSE: 对应于数据集配置是否正确。
  return.value <- TRUE
  src <- NULL
  if(is.null(data.conf$name) || is.null(data.conf$type)) {
    warning("Configuration of data set must specify name and type properties!")
    return(FALSE)
  }
  if(!is.null(data.conf$src)) src <- data.conf$src
  LoadDataSetGenerator(data.conf)
  if ( exists(paste(data.conf$name, 'validation', sep='.')) ) {
    validation.function <- get(paste(data.conf$name, 'validation', sep='.'))
    return.value <- validation.function(data.conf)
  } else {
    return.value <- TRUE
  }
  return(return.value)
}

ResolveShortcutInDataConf <- function(data.conf) {
  # 解析数据集配置中的快捷配置。
  #
  # 快捷配置对应于数据集特定的一组配置取值。
  # 基于快捷方式，可以避免用户在任务配置文件中过多地书写配置代码，
  # 进而产生错误。
  #
  # 本函数将数据配置中的快捷配置解析成规范配置，并重新放入到数据集
  # 配置中。
  #
  # Args:
  #   data.conf: 
  LoadDataSetGenerator(data.conf)
  if ( exists(paste(data.conf$name, "conf", "shortcuts", sep='.'))) {
      shortcuts <- get(paste(data.conf$name, "conf", "shortcuts", sep='.'))
      data.conf <- ResolveConfigurationShortcut(data.conf, shortcuts)
  } 
  return(data.conf)
}


GetExternalDataSet <- function(foldername, filename = NULL, header = F, sep=""){
  # 得到外部文件所在路径.
  # 目前支持：
  #     1.文件已文本文件的形式存储在硬盘上。环境变量SXU_SWL_DATASETS_PATH指向文件存储的路径。
  # 
  # Args:
  #   folder.name: 文件所在目录名称；这里规定文件名(无后缀或后缀为.data)与目录名相同;
  # Returns:
  #   文件所在的绝对路径.
  #
  # TODO(wangruibo@2017/5/30): 支持从数据库中读取数据表;
  # TODO(wangruibo@2017/5/30): 支持从Redis中读取数据表;
  data_dirpath = "/public/swl/datasets"
  if ( "" != Sys.getenv("SXU_SWL_DATASETS_PATH") )
    data_dirpath = Sys.getenv("SXU_SWL_DATASETS_PATH")
  if(is.null(filename) ) 
    filename = foldername
  path <- file.path(data_dirpath, foldername, filename)
  path_data <- file.path(data_dirpath, foldername, paste(filename, ".data", sep=""))
  if(file.exists(path) || file.exists(path_data)) {
    if (file.exists(path)) {
      data <- read.table(path, header=header, sep = sep)
      return(data)
    }
    data <- read.table(path_data, header=header, sep = sep) 
    return(data)
  }
  else
    stop('SXU_SWL_DATASETS_PATH is wrong.')
}


ResampleObservantsFromPopulationWithAdvancement <- function(dataset, resample.config){
  # 从有限样本中按照重抽样配置，抽取一组数据出来。
  #
  # 重抽样配置中，给出了如下多种抽样方式:
  # 1. 无放回抽样:
  #    1) y不均衡：直接从整个样本集抽样;
  #    2) y均衡: 从整个样本集中按照y的分布进行抽样;
  #       若by.portion为true,则按给定的笔记抽样;
  # 2. 有放回抽样:
  #    1) y不均衡：直接从整个样本集抽样;
  #    2) y均衡: 从整个样本集中按照y的分布进行抽样;
  #       若by.portion为true,则按给定的笔记抽样;
  # Args:
  #   dataset: 被当做总体的数据集.
  #   resample.config: 重抽样配置,其中包含: n(抽样个数)、yBalance(是否按y分布均衡抽样)、
  #                    byPortion(是否按给定比例抽样)、portionName(类别名称)、portionValue(类别比例).
  #
  # Returns:
  #   抽样后的数据集
  #
  n <-  0
  if(is.null(resample.config$n)) {
     stop("param n should be contained in resample configuration")
  }
  n <- resample.config$n  # 要抽取的样本个数
  y.balance <- FALSE  # y是否均衡
  if (!is.null(resample.config$yBalance)) {
    y.balance <- resample.config$yBalance 
  }
  by.portion <- FALSE  # 是否要按比例抽取
  if (!is.null(resample.config$byPortion)) {
    by.portion <- resample.config$byPortion
  }
  portions <- NULL
  if(by.portion == TRUE) {  # 如果by.portion为真，则需要提供portions参数.
    if(is.null(resample.config$portionName) || is.null(resample.config$portionValue)) {
      stop("resample portions are not provided.")
    } else {
      portions.dim <- resample.config$portionName
      portions.value <- resample.config$portionValue
    }
  }
  sample.replace <- TRUE
  if (!is.null(resample.config$replace)) {
    sample.replace <- resample.config$replace  # 是否有放回抽取
  }
  population.size <- nrow(dataset)
  observants <- NULL
  # 首先，区分是否是有放回抽样;
  if(y.balance == FALSE) {  # 不考虑Y的分布情况
    # 判断抽样个数是否小于样本总个数
    if(n > population.size && !sample.replace) {
      stop("Without replacement sampling, sample size should not larger than population size.")
    }
    indices <- sample(1:population.size, size=n, replace = sample.replace)
    observants <- dataset[indices,]
    row.lables <- row.names(observants)
    if(!is.null(row.lables) && length(row.lables) > 0) {
      row.names(observants) <- 1:nrow(observants)
    }
    return(observants)
  } else {  # 考虑Y的分布情形
    y <- dataset[,ncol(dataset)]
    if(by.portion == FALSE) {
      # 如果by.portion参数为FALSE, 则按照y分布的自然比例进行抽样;
      portions.table <- table(y)/population.size
      portions.dim <- c(dimnames( portions.table)$y)
      dimnames(portions.table)<-list()
      portions.value <- portions.table
    } else {
      # 如果by.portion参数为TRUE,则按照所提供的比例进行抽样;
      class.unique <- unique(y)
      class.sort <- sort(class.unique)
      portions.dim.sort <- sort(portions.dim)
      if (!identical(class.sort, portions.dim.sort)){
        stop("class names are not provided correctly in resample config.")
      } 
      if(length(portions.dim) != length(portions.value)) {
        stop("class length is not equal to portion values.")
      }
      # todo: 检验portion value中加起来是否为1.
    }
    # 为每一类生成样本
    class.length <- length(portions.dim)
    for(class.index in 1:class.length) {
       class.name <- portions.dim[class.index]
       #todo: 这可能会出现所有个数不足n的情形。
       class.count <- round(portions.value[class.index] * n)
       class.population.indices <- which(y==class.name)
       class.population.size <- length(class.population.indices)
       if(class.count > class.population.size && !sample.replace) {
         stop("resampled class count is too large")
       }
       class.observants <- dataset[sample(class.population.indices, size = class.count, replace = sample.replace),]
       observants <- rbind(observants, class.observants)
    }
    observants.size <- nrow(observants)
    if(n != observants.size) {
      warning(paste("generate unenough samples with resampling method:", observants.size))
    }
    observants <- observants[sample(1:observants.size, size=observants.size, replace = FALSE),]  # 打乱样本
    row.lables <- row.names(observants)
    if(!is.null(row.lables) && length(row.lables) > 0) {
      row.names(observants) <- 1:nrow(observants)
    }
    return(observants)
  }
  return(observants)
}


loadPopulationDataFromExternal <- function (foldername, datafilename=NULL, na.strings="NA", omitNArow = FALSE) {
  path <- GetDatafilepath(foldername, datafilename)
  data<-read.table(path, sep=",", header=FALSE, na.strings = na.strings, strip.white = TRUE)
  if(omitNArow)
    data <- na.omit(data)
  x<-data[,1:ncol(data)-1]
  y<-data[,ncol(data)]
  data_n <- as.data.frame(cbind(x,y))  
  return(data_n)
}

StatBasicInfoOfDataSet <- function(dataset.conf) {
  # 给定数据集的配置，统计所生成数据集的基本信息。
  #
  # 基本信息主要包括:
  #    - 数据集的大小
  #    - 自变量的个数
  #    - 因变量中类别的个数（实数值变量暂不考虑）
  #
  # Args:
  #   dataset.conf: 正确的数据配置。
  #
  # Returns:
  #   数据的基本配置，以table列出。
  dataset.object <- LoadDataSetGenerator(dataset.conf)
  dataset.package <- dataset.object[[2]]
  dataset.generator <- dataset.object[[1]]
  WorkerInit(dataset.package)
  dataset <- dataset.generator(dataset.conf)
  # 判断是不是分类数据
  is_classification <- is.factor(dataset[,ncol(dataset)])
  if(!is_classification)
    stop(paste(dataset.conf$name, "This function is merely suitable to classification data set."))
  dataset.info <- c(dataset.conf[["name"]])
  dataset.info <- c(dataset.info, nrow(dataset))
  dataset.info <- c(dataset.info, ncol(dataset)-1)
  dataset.info <- c(dataset.info, length(levels(dataset[,ncol(dataset)])))
  dataset.info <- t(dataset.info)
  colnames(dataset.info) <- c("Name", "n", "#predictors", "#Classes")
  return(dataset.info)
}