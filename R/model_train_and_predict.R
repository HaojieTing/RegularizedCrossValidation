# 模型训练和预测模块。
#
# 这里主要集中了关于切分的一些操作、模型训练和预测的一些公用函数。
# 具体提供了如下几方面的功能:
# 1. 给出数据集上某一个算法的Hold-out估计.
# 2. 给出数据集上某一个算法的Resampling估计。
# 3. 给出数据集上的某个算法的Re-substitution估计（封闭预测）。
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/5/9

SwapObservantsToConstructNewPartition <- function(partition, swap.count){
  # 对给定的切分，将训练集和验证集交换指定的观测个数，
  # 形成新的切分并返回。
  #
  # 交换所用的具体规则是，训练集的后几个观测和验证集的前几个观测
  # 交换，形成新的切分。
  #
  # 需要注意：
  #     1. 调用两次本函数，得到相同的切分。
  #     2. 本函数的切分可以是任意的list,不一定必须是切分(这种情况下，函数的涵义有所延伸)。
  #
  # Args:
  #   partition: 数据切分，为list类型，含两个元素:
  #              第一个元素存储训练集的指标，第二个元素存储测试集的指标。
  #   swap.count: 要交换的观测个数。
  #
  # Returns:
  #   新的切分，为含2个元素的list.
  partition.size <- length(partition)
  if (partition.size != 2) {
    stop("The partition for swaping should only contain two elements!")
  }
  if (swap.count == 0)
    return(partition)
  partition.new <- vector("list",partition.size)  
  training.index <- partition[[1]]
  validating.index <- partition[[2]]
  training.total.count <- length(training.index)
  training.remain.count <- training.total.count - swap.count
  training.part.remain <- c()
  if (training.remain.count >= 1) {
    training.part.remain <- training.index[1:training.remain.count]
  }
  training.part.swap <- c()
  if (training.total.count >= (training.remain.count+1))  {
    training.part.swap <- training.index[(training.remain.count+1):training.total.count]
  }
  validating.total.count <- length(validating.index)
  validating.part.swap <- c()
  if (swap.count >=1) {
    validating.part.swap <- validating.index[1:swap.count]
  }
  validating.part.remain <- c()
  if (validating.total.count >= (swap.count+1)) {
    validating.part.remain <- validating.index[(swap.count+1):validating.total.count]
  }
  partition.new[[1]] <- c(training.part.remain, validating.part.swap)
  partition.new[[2]] <- c(training.part.swap, validating.part.remain)
  return(partition.new)    
}

randomExchangeingIndexes <- function(n, n1, cnt) {
  n2 <- n -  n1
  elesIngroup1<-sample(n1, cnt)
  elesIngroup2<-sample(n2, cnt)
  return(list(eci_train = elesIngroup1,
              eci_test = elesIngroup2))
}

reOrderGroups <- function(groups, cnt, eci) {
  n_group <- length(groups)  
  if(n_group != 2)
    stop("this function is only suitable to 2-group-situation")
  if(cnt <= 0)
    return(groups)
  newGps <- vector("list",n_group)
  elesIngroup1<- eci$eci_train
  elesIngroup2<- eci$eci_test
  eles1<-groups[[1]][elesIngroup1]
  eles2<-groups[[2]][elesIngroup2]
  elesremain1<-groups[[1]][-elesIngroup1]
  elesremain2<-groups[[2]][-elesIngroup2]
  if(length(elesremain1)!=0 && length(eles2)!=0)
    newGps[[1]]<-append(elesremain1,eles1)
  if(length(elesremain2)!=0 && length(eles1)!=0)
    newGps[[2]]<-append(eles2, elesremain2)
  if(length(elesremain1)==0)
    newGps[[1]]<-eles1
  if(length(eles2)==0)
    newGps[[1]]<-elesremain1
  if(length(elesremain2)==0)
    newGps[[2]]<-eles2
  if(length(eles1)==0)
    newGps[[2]]<-elesremain2  
  #newGps[[1]]<-sort(newGps[[1]])
  #newGps[[2]]<-sort(newGps[[2]])
  return(newGps)  
}


randomExchangeingObservants <- function(groups, cnt, eci) {
  n_group <- length(groups)  
  if(n_group != 2)
    stop("this function is only suitable to 2-group-situation")
  if(cnt <= 0)
    return(groups)
  newGps <- vector("list",n_group)
  elesIngroup1<- eci$eci_train
  elesIngroup2<- eci$eci_test
  eles1<-groups[[1]][elesIngroup1]
  eles2<-groups[[2]][elesIngroup2]
  elesremain1<-groups[[1]][-elesIngroup1]
  elesremain2<-groups[[2]][-elesIngroup2]
  if(length(elesremain1)!=0 && length(eles2)!=0)
    newGps[[1]]<-append(elesremain1,eles2)
  if(length(elesremain2)!=0 && length(eles1)!=0)
    newGps[[2]]<-append(eles1, elesremain2)
  if(length(elesremain1)==0)
    newGps[[1]]<-eles2
  if(length(eles2)==0)
    newGps[[1]]<-elesremain1
  if(length(elesremain2)==0)
    newGps[[2]]<-eles1
  if(length(eles1)==0)
    newGps[[2]]<-elesremain2  
  #newGps[[1]]<-sort(newGps[[1]])
  #newGps[[2]]<-sort(newGps[[2]])
  return(newGps)
}

#This function is used for randomly exchanging several elements in groups, and the resulting
#groups will keep straitifing too.
randomExchangeingObservantsStratified<-function(groups, posIndexArray, negIndexArray, cnt){
  n_group <- length(groups)  
  if(n_group != 2)
    stop("this function is only suitable to 2-group-situation")
  if(cnt <= 0)
    return(groups)
  newGps <- vector("list",n_group)
  elesIngroup1<-sample(length(groups[[1]]),cnt)
  eles1<-groups[[1]][elesIngroup1]
  elesremain1<-groups[[1]][-elesIngroup1]
  #compute the positive sample count and negative sample count in  eles1
  positivecnt<-length(intersect(eles1, posIndexArray))
  negitivecnt<-cnt -positivecnt
  eles2<-append(sample(intersect(groups[[2]],posIndexArray),positivecnt), sample(intersect(groups[[2]],negIndexArray),negitivecnt))
  elesremain2<-setdiff(groups[[2]],eles2)
  if(length(elesremain1)!=0 && length(eles2)!=0)
    newGps[[1]]<-append(elesremain1,eles2)
  if(length(elesremain2)!=0 && length(eles1)!=0)
    newGps[[2]]<-append(elesremain2,eles1)
  if(length(elesremain1)==0)
    newGps[[1]]<-eles2
  if(length(eles2)==0)
    newGps[[1]]<-elesremain1
  if(length(elesremain2)==0)
    newGps[[2]]<-eles1
  if(length(eles1)==0)
    newGps[[2]]<-elesremain2  
  newGps[[1]]<-sort(newGps[[1]])
  newGps[[2]]<-sort(newGps[[2]])
  return(newGps)
}

###############################
# generate simulation dataset.
###############################
yieldToySamples<-function(sflag,N,d, rpt, sampleFun,curDirIdx='0'){
  dtdir =  paste(getwd(),'/sdatas/',sep='')
  samples <- vector("list",rpt)
  rdir=paste('/sdatas/',curDirIdx,sep='')
  rdir=paste(rdir,'/',sep='')
  cdtdir = paste(getwd(),rdir,sep='')
  if(sflag==0)
  {
    for(iter in 1:rpt){
      data <- sampleFun(N, d)
      samples[[iter]] <-data
    }
   #    dtIdx = length(list.dirs(dtdir))-1
    dir.create(paste(dtdir,dtIdx, sep=''))   
    cdtdir = paste(dtdir,dtIdx, sep='')
    cdtdir = paste(cdtdir , "/", sep='')
    #tfn = paste(dtdir,paste(dtIdx,"/data.rdata",sep=''),sep='')
    save(samples, rpt, file=dtfn)
  }
  if(sflag==1){ 
    load(paste(cdtdir, "data.rdata",sep='')) 
  }
  return(list(samples,cdtdir))
}


trainAndTestRepeatlyByExchangeCount<-function(samples, rpt, d, cnt, groups,classifier,V=2){
  loss_rpt = vector("list",length(rpt))
  regroups<-exchangingObservants(groups,cnt)
  for(r in 1:rpt)
  {
    if(r%%1000 == 0) print(paste(cnt,r))
    data<-samples[[r]]
    x<-matrix(c(as.matrix(data[[1]])),ncol=d)
    y<-matrix(c(as.matrix(data[[2]])),ncol=1)
    loss_V=vector("list",V)
    for(v in 1:V){
      pre<-trainAndTest(x[-regroups[[v]],],y[-regroups[[v]]],x[regroups[[v]],],classifier)      
      loss = abs(pre-y[regroups[[v]]])
      loss_V[[v]] <- loss
    }
    nloss_V <-  exchangingObservants(loss_V,cnt)
    ls <-append(nloss_V[[1]],nloss_V[[2]])
    loss_rpt[[r]] <- ls
  }
  return(loss_rpt)
}


trainAndTestRepeatly<-function(samples,rpt,d,groups,classifier,V=2){  
  loss_rpt = vector("list",length(rpt))
  for(r in 1:rpt)
  {
    if(r%%1000 == 0) print(paste(r))
    data<-samples[[r]]    
    x<-matrix(c(as.matrix(data[[1]])),ncol=d)
    y<-matrix(c(as.matrix(data[[2]])),ncol=1)
    loss_V=vector("list",V)
    for(v in 1:V){      
      pre<-trainAndTest(x[-groups[[v]],],y[-groups[[v]]],x[groups[[v]],],classifier)      
      loss = abs(pre-y[groups[[v]]])
      loss_V[[v]] <- loss
    }
    ls <-append(loss_V[[1]],loss_V[[2]])
    loss_rpt[[r]] <- ls
  }
  return(loss_rpt)
}

trainAndTestForOneSampleAndOneCVSplit<-function(sample,d, groups, classifier, V=2){
  data<-sample
  x<-matrix(c(as.matrix(data[[1]])),ncol=d)
  y<-matrix(c(as.matrix(data[[2]])),ncol=1)
  loss_V=vector("list",V)
  for(v in 1:V){      
    pre<-trainAndTest(x[-groups[[v]],],y[-groups[[v]]],x[groups[[v]],],classifier)      
    loss = abs(pre-y[groups[[v]]])
    loss_V[[v]] <- loss
  }
  ls <-append(loss_V[[1]],loss_V[[2]])
  return(ls)
}

trainAndTestForMultiGroupsOfCV<-function(sample, m, d, mgroupsList, classifier, V=2){
  lossList<-vector("list",m)
  for(i in 1:m){
    groups<-mgroupsList[[i]]
    ls <- trainAndTestForOneSampleAndOneCVSplit(sample,d, groups,classifier, V)
    lossList[[i]] <- ls
  }
  return(lossList)
}

TrainAndTestForResubstitutionEstimator <- function(dataset, partition, algorithm.entry, 
                                                   algorithm.conf, metric.entry.list =NULL, 
                                                   metric.conf.list = NULL) {
  # 给定算法、数据集、切分，得到相应的Re-substitution估计。
  #
  # Args:
  #   dataset: 数据集的所有观测.
  #   partition: 数据切分方式,格式为含有2个元素的list格式，
  #              第1个元素为训练指标集，第2个元素为测试指标集.
  #   algorithm.entry: 算法训练测试入口函数，包括训练和测试两个阶段.
  #   algorithm.conf: 机器学习算法的配置.
  #   reverse(deprecated): 是否反过来训练，如果反过来训练，则将测试集当作训练集，
  #                        将训练集当成测试集，形成第2个Holdout估计。
  #   metric.entry.list : 性能评价指标的入口函数列表（一般对应于生成函数)
  #   metric.conf.list  : 性能评价指标的配置列表
  # Returns:
  #   1. Resubstitution估计值.
  #   2. Resubstitution估计值对应的损失向量（如果存在的话）.
  
  # STEP1: 验证性能评价指标列表是否正确
  if(!is.null(metric.entry.list) || !is.null(metric.conf.list)) {
    if(!is.list(metric.entry.list) || !is.list(metric.conf.list)){
      stop("metric conf and entry should be list type.")
    }
    if(length(metric.entry.list) != length(metric.conf.list)) {
      stop("count of entries is not equal to that of confs")
    }
  }
  
  #STEP2: 训练模型，并测试获取实验结果
  algorithm.type <- algorithm.conf$type
  ho1.loss <- NA
  ho1.est <- NA
  result.list <- list()
  train.dataset <- dataset
  if(length(partition) >= 1) {
    train.dataset <- dataset[partition[[1]],]
  }
  response.predict <- algorithm.entry(train.dataset, train.dataset, algorithm.conf)
  response.golden <- train.dataset[, ncol(dataset)]
  
  #STEP3: 计算评价指标估计，并返回
  if(is.null(metric.entry) && is.null(metric.conf)) {
    # 默认的评价指标（在没有提供评价指标的前提下，每种任务只计算一个评价指标）：
    #     分类任务: 0-1损失;
    #     回归任务: 平方损失;
    if (algorithm.type == 'classification') {
      ho1.loss <- abs( response.predict != response.golden)   
    } else {
      ho1.loss <- (response.predict -  response.golden)**2      
    }
    ho1.est <- mean(ho1.loss)
    result.list[[1]] <- ho1.est
    result.list[[2]] <- ho1.loss
  } else {
    metrics.length <- length(metric.entry.list) #要计算的评价指标列表的长度
    for(i in 1:metrics.length) { # 一个机器学习模型可以使用多种评价指标来度量其性能。
      metric.entry <- metric.entry.list[[i]]
      metric.conf  <- metric.conf.list[[i]]
      metric.conf$gold <- response.golden
      metric.conf$pre <- response.predict
      metric.conf.name <- metric.conf$name # 获取评价指标配置文件
      result.list[[metric.conf.name]] <- metric.entry(metric.conf)
    }
  }
  return(result.list)
}

TrainAndTestForHoldoutEstimator <- function(dataset, partition, algorithm.entry, 
                                            algorithm.conf, metric.entry=NULL, 
                                            metric.conf = NULL){
  # 给定算法、数据集、切分，得到相应的hold-out中各观测点的损失值及最终的估计。
  #
  # Args:
  #   dataset: 数据集的所有观测.
  #   partition: 数据切分方式,格式为含有2个元素的list格式，
  #              第1个元素为训练指标集，第2个元素为测试指标集.
  #   algorithm.entry: 算法训练测试入口函数，包括训练和测试两个阶段.
  #   algorithm.conf: 机器学习算法的配置.
  #   reverse(deprecated): 是否反过来训练，如果反过来训练，则将测试集当作训练集，
  #                        将训练集当成测试集，形成第2个Holdout估计。
  #   metric.entry: 性能评价指标的入口函数（一般对应于生成函数)
  #   metric.conf : 性能评价指标的配置
  # Returns:
  #   1. hold-out估计值1.
  #   2. hold-out估计值1对应的损失向量（如果存在的话）.
  #
  # TODO(wangruibo@2017/5/10): 将损失函数替换成可以自定义的评价指标.
  
  
  algorithm.type <- algorithm.conf$type
  ho1.loss <- NA
  ho1.est <- NA
  result.list <- list()
  response.predict <- algorithm.entry(dataset[partition[[1]],], dataset[partition[[2]],], algorithm.conf)
  response.golden <- dataset[partition[[2]], ncol(dataset)]
  if(is.null(metric.entry) && is.null(metric.conf)) {
    # 默认的评价指标：
    #     分类任务: 0-1损失;
    #     回归任务: 平方损失;
    
    if(length(response.predict) / length(partition[[2]])==1 && length(response.predict) %% length(partition[[2]])==0) {
      if (algorithm.type == 'classification') {
        levels(response.predict) <- levels(response.golden)
        ho1.loss <- abs( response.predict != response.golden)   
      } else {
        ho1.loss <- (response.predict -  response.golden)**2      
      }
      ho1.est <- mean(ho1.loss)
      result.list[[1]] <- ho1.est
      result.list[[2]] <- ho1.loss
    } else if(length(response.predict) / length(partition[[2]])>1 && length(response.predict) %% length(partition[[2]])==0) {
      if (algorithm.type == 'classification') {
        levels(response.predict) <- levels(response.golden)
        ho1.loss <- apply(response.predict, 1, function(x){ return(x != response.golden)})   
      } else {
        ho1.loss <- apply(response.predict, 1, function(x){return((x-response.golden)**2)})      
      }
      ho1.est <- rowMeans(ho1.loss)
      result.list[[1]] <- ho1.est
      result.list[[2]] <- ho1.loss
    } else {
      stop(paste("Unsupported prediction format:",length(response.predict), length(partition[[2]])))
    }
  } else {
      metric.conf$gold <- response.golden
      metric.conf$pre <- response.predict
      metric.conf.name <- metric.conf$name # 获取评价指标配置文件
      result.list <- metric.entry(metric.conf)
  }
  return(result.list)
}

TrainAndTestForCrossValidationEstimatorWithPartitionSet <- function(dataset, partition.set,
                                                                    algorithm.entry, algorithm.conf, 
                                                                    metric.entry=NULL, 
                                                                    metric.conf=NULL) {
  # 给定数据集，切分集合以及机器学习算法，计算交叉验证估计。
  #
  # 具体计算出切分集合中每个切分对应的holdout估计，并对所有的
  # holdout估计平均，得到最终的交叉验证估计。
  #
  # Args: 
  #   dataset: 数据集
  #   partition.set: 切分集合
  #   algorithm.entry: 机器学习算法入口函数，包括训练和测试两个阶段.
  #   algorithm.conf: 机器学习算法配置
  #
  # Returns:
  #   1. 交叉验证估计.
  #   2. hold-out估计向量.
  algorithm.type <- algorithm.conf$type
  partition.set.size <- length(partition.set)
  holdout.estimator.vector<- c()
  # 对于每一个切分,得到hold-out估计。
  result.aggr.list <- list()    # 存储实验结果聚合后的结果
  result.verbose.list <- list() # 存储实验结果的详情（未聚合）
  for(i in 1:partition.set.size) {
    partition.current <- partition.set[[i]]  # 得到当前的切分
    # 得到当前切分对应的hold-out估计
    holdout.est.verbose <- TrainAndTestForHoldoutEstimator(dataset, partition.current, algorithm.entry, 
                                                           algorithm.conf, 
                                                           metric.entry = metric.entry, 
                                                           metric.conf = metric.conf)
      holdout.est <- holdout.est.verbose[[1]]
      if(length(holdout.est) == 1) {
        holdout.estimator.vector <- c( holdout.estimator.vector, holdout.est)
      } else {
        holdout.estimator.vector <- rbind(holdout.estimator.vector, holdout.est)
      }
  }
  crossvalidation.estimator <- c()
  if(is.vector(holdout.estimator.vector)) {
    crossvalidation.estimator <- mean(holdout.estimator.vector)
  } else if(is.vector(holdout.estimator.vector)) {
    crossvalidation.estimator <- colMeans(holdout.estimator.vector)
  } else {
    stop("Unsupported format")
  }
  return(list(crossvalidation.estimator, holdout.estimator.vector))
}