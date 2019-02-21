# 计算TKDE 2015上的实验。
# 仅考虑Simulation Data Comparison.
# 仅考虑 3x2. logistic regression
# 1. 生成训练集n=200个训练样本.
# 2. 生成测试集n=1000个测试样本.
# 3. 使用logistic regression训练模型并在测试集上测试，得到真实f值。
# 4. 在训练集抽数据重复1000次，并计算每一次的混淆矩阵。
# 5. 基于混淆矩阵计算置信区间，并计算置信区间长度及包含真实值次数。
library(extraDistr)

measure.P.credible.interval <- function(confusion.matrix, lambda =1, alpha = 0.05) {
  tp <- confusion.matrix[1]
  fp <- confusion.matrix[2]
  fn <- confusion.matrix[3]
  tn <- confusion.matrix[4]
  ci.lower <- qbeta(alpha/2, tp+lambda, fp+lambda)
  ci.upper <- qbeta(alpha/2, tp+lambda, fp+lambda, lower.tail = F)
  return(c(ci.lower, ci.upper))
}

measure.R.credible.interval <- function(confusion.matrix, lambda =1, alpha = 0.05) {
  tp <- confusion.matrix[1]
  fp <- confusion.matrix[2]
  fn <- confusion.matrix[3]
  tn <- confusion.matrix[4]
  ci.lower <- qbeta(alpha/2, tp+lambda, fn+lambda)
  ci.upper <- qbeta(alpha/2, tp+lambda, fn+lambda, lower.tail = F)
  return(c(ci.lower, ci.upper))
}

measure.F1.credible.interval <- function(confusion.matrix, lambda =1, alpha = 0.05) {
  tp <- confusion.matrix[1]
  fp <- confusion.matrix[2]
  fn <- confusion.matrix[3]
  tn <- confusion.matrix[4]
  betapr.ci.lower <- qbetapr(alpha/2, fn+fp+2*lambda, tp+lambda)
  betapr.ci.upper <- qbetapr(alpha/2, fn+fp+2*lambda, tp+lambda, lower.tail = F)
  f1.ci.lower <- 1.0/(1.0+0.5*betapr.ci.upper)
  f1.ci.upper <- 1.0/(1.0+0.5*betapr.ci.lower)
  return(c(f1.ci.lower, f1.ci.upper))
}

TrainAndTestForPRF <- function(train.dataset, test.dataset, algorithm.entry, 
                                            algorithm.conf){
  algorithm.type <- algorithm.conf$type
  ho1.loss <- NA
  ho1.est <- NA
  result.list <- list()
  response.predict <- algorithm.entry(train.dataset, test.dataset, algorithm.conf)
  response.golden <- test.dataset[, ncol(test.dataset)]
  if(length(response.predict) == length(response.predict)) {
    if (algorithm.type == 'classification') {
      levels(response.predict) <- levels(response.golden)
      confusion.matrix <- table(response.predict, response.golden)
      tp <- confusion.matrix[2, 2]
      tn <- confusion.matrix[1, 1]
      fp <- confusion.matrix[2, 1]
      fn <- confusion.matrix[1, 2]
      p = tp*1.0/(tp+fp)
      r = tp*1.0/(tp+fn)
      f = 2*p*r/(p+r)
      return(list(tp=tp,fp=fp,fn=fn,tn=tn, p=p, r=r, f=f))
    } else {
      stop("Unsupported")
    }
  } else {
    stop(paste("Unsupported prediction format:",length(response.predict), length(partition[[2]])))
  }
  return(result.list)
}


setwd(file.path(getwd(),"R"))
source("./utils.R", encoding="UTF-8")
source("./datasets/classification/two_normals_classification.R", encoding = "UTF-8")
source("./mlAlgorithms/algor_loader.R", encoding="UTF-8")
source("./crossvalidations/cv_loader.R", encoding="UTF-8")
index.P.15 = 0
index.R.15 = 0
index.F1.15 = 0
len.P.15 = 0.0
len.R.15 = 0.0
len.F1.15 = 0.0
index.P.now = 0
index.R.now = 0
index.F1.now = 0
len.P.now = 0.0
len.R.now = 0.0
len.F1.now = 0.0
train.data.size = 100
sim.count = 1000
test.data.size = 5* train.data.size 
for(repi in 1:sim.count) {
  if (repi %% 10 == 0) print(repi)
  train.dataset.conf <-list(n0 = 300, n1=300, mu0 = rep(0,2), mu1 = rep(0.5,2), sigma0 = diag(2), sigma1 = diag(2))
  train.dataset <- two_normals_classification.DataGenerator(train.dataset.conf)
  test.dataset.conf <-list(n0 = 1500, n1=1500, mu0 = rep(0,2), mu1 = rep(0.5,2), sigma0 = diag(2), sigma1 = diag(2))
  test.dataset <- two_normals_classification.DataGenerator(test.dataset.conf)
  algorithm.conf <- list(name="logisticGLM", type = "classification", method="class")
  algorithm.info <- LoadAlgorithmGenerator(algorithm.conf)
  algorithm.generator <- algorithm.info[[1]]
  algorithm.packages <- algorithm.info[[2]]
  WorkerInit(algorithm.packages)
  true.values <- TrainAndTestForPRF(train.dataset, test.dataset, algorithm.generator, algorithm.conf)
  true.value.P <- true.values$p
  true.value.R <- true.values$r
  true.value.F1 <- true.values$f
#在训练集上实施交叉验证.
  b3x2cv.conf <- list(m=3, name="mx2bcv_inc",data=train.dataset)
  crossvalidation.info <- LoadCrossValidationGenerator(b3x2cv.conf$name)
  crossvalidation.generator <- crossvalidation.info[[1]]
  crossvalidation.packages <- crossvalidation.info[[2]]
  cv.object <- crossvalidation.generator(b3x2cv.conf)
  partitionset <- cv.object$partitions
  confusion.matrices <- matrix(rep(0, length(partitionset)*4), ncol = 4)
  for(index in 1:length(partitionset)) {
    partition <- partitionset[[index]]
    train.indices <- partition[[1]]
    validation.indices <- partition[[2]]
    cv.train.dataset <- train.dataset[train.indices,]
    cv.validation.dataset <- train.dataset[validation.indices,]
    result <- TrainAndTestForPRF(cv.train.dataset, cv.validation.dataset, algorithm.generator, algorithm.conf)
    confusion.matrices[index, ] <- c(result$tp, result$fp, result$fn, result$tn)
  }
  # 15年王钰师兄的计算方法：平均所有的混淆矩阵。
  confusion.matrix.avg.15 <- colMeans(confusion.matrices)
  ci.P.15 <- measure.P.credible.interval(confusion.matrix.avg.15)
  ci.R.15 <- measure.R.credible.interval(confusion.matrix.avg.15)
  ci.F1.15 <- measure.F1.credible.interval(confusion.matrix.avg.15)
  # 求取置信区间长度。
  ci.len.P.15 <- ci.P.15[2]-ci.P.15[1]
  ci.len.R.15 <- ci.R.15[2]-ci.R.15[1]
  ci.len.F1.15 <- ci.F1.15[2]-ci.F1.15[1]
  if(true.value.P >= ci.P.15[1] && true.value.P <= ci.P.15[2]) {
    index.P.15 = index.P.15 + 1
  }
  if(true.value.R >= ci.R.15[1] && true.value.R <= ci.R.15[2]) {
    index.R.15 = index.R.15 + 1
  }
  if(true.value.F1 >= ci.F1.15[1] && true.value.F1 <= ci.F1.15[2]) {
    index.F1.15 = index.F1.15 + 1
  }
  len.P.15 = len.P.15 + ci.len.P.15
  len.R.15 = len.R.15 + ci.len.R.15
  len.F1.15 = len.F1.15 + ci.len.F1.15
  # 现在的计算方法
  confusion.matrix.now <- colSums(confusion.matrices) * 0.3688
  ci.P.now <- measure.P.credible.interval(confusion.matrix.now)
  ci.R.now <- measure.R.credible.interval(confusion.matrix.now)
  ci.F1.now <- measure.F1.credible.interval(confusion.matrix.now)
  ci.len.P.now <- ci.P.now[2]-ci.P.now[1]
  ci.len.R.now <- ci.R.now[2]-ci.R.now[1]
  ci.len.F1.now <- ci.F1.now[2]-ci.F1.now[1]
  if(true.value.P >= ci.P.now[1] && true.value.P <= ci.P.now[2]) {
    index.P.now = index.P.now + 1
  }
  if(true.value.R >= ci.R.now[1] && true.value.R <= ci.R.now[2]) {
    index.R.now = index.R.now + 1
  }
  if(true.value.F1 >= ci.F1.now[1] && true.value.F1 <= ci.F1.now[2]) {
    index.F1.now = index.F1.now + 1
  }
  len.P.now = len.P.now + ci.len.P.now
  len.R.now = len.R.now + ci.len.R.now
  len.F1.now = len.F1.now + ci.len.F1.now
}
index.P.15 = index.P.15 /sim.count
index.R.15 = index.R.15 /sim.count
index.F1.15 = index.F1.15 /sim.count
index.P.now = index.P.now /sim.count
index.R.now = index.R.now /sim.count
index.F1.now = index.F1.now /sim.count
len.P.15 = len.P.15/sim.count
len.R.15 = len.R.15/sim.count
len.F1.15 = len.F1.15/sim.count
len.P.now = len.P.now/sim.count
len.R.now = len.R.now/sim.count
len.F1.now = len.F1.now/sim.count
print(c(true.value.P, true.value.R, true.value.F1))
print(c(index.P.15, index.R.15, index.F1.15, len.P.15, len.R.15, len.F1.15))
print(c(index.P.now, index.R.now, index.F1.now, len.P.now, len.R.now, len.F1.now))