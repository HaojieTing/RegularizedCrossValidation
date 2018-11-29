source("./crossvalidations/cv_loader.R",encoding = "UTF-8")
source("utils.R", encoding = "UTF-8")

MeanOfOverlappingNumber <- function(cvConf, sim.count = 1000) {
  mean.vector <- c()
  for(i in 1:sim.count) {
    generator <- LoadCrossValidationGenerator(cvConf$name)
    cv.function <- generator[[1]]
    partition.set <- cv.function(cvConf)
    partition.matrix <- convertParitionSetToPartitionMatrix(partition.set)
    overlapping.numbers <- overlappingCountsOfPartitionSet(partition.matrix)
    diag(overlapping.numbers) <- NA
    mean.vector <- append(mean.vector,mean(overlapping.numbers, na.rm = TRUE))
  }
  return(mean(mean.vector))
}

MediaOfOverlappingNumber <- function(cvConf, sim.count = 1000) {
  numbers.vector <- c()
  for(i in 1:sim.count) {
    generator <- LoadCrossValidationGenerator(cvConf$name)
    cv.function <- generator[[1]]
    partition.set <- cv.function(cvConf)
    partition.matrix <- convertParitionSetToPartitionMatrix(partition.set)
    overlapping.numbers <- overlappingCountsOfPartitionSet(partition.matrix)
    numbers.vector <- append(numbers.vector,overlapping.numbers[lower.tri(overlapping.numbers)])
  }
  return(median(numbers.vector))
}

MeanOfOverlappingNumberDiff <- function(cvConf, sim.count = 1000) {
  mean.vector <- c()
  for(i in 1:sim.count) {
    generator <- LoadCrossValidationGenerator(cvConf$name)
    cv.function <- generator[[1]]
    partition.set <- cv.function(cvConf)
    partition.matrix <- convertParitionSetToPartitionMatrix(partition.set)
    overlapping.numbers <- overlappingCountsOfPartitionSet(partition.matrix)
    overlapping.numbers <- overlapping.numbers[lower.tri(overlapping.numbers)]
    vector.grid <- expand.grid(overlapping.numbers,overlapping.numbers)
    mean.one <- mean(abs(vector.grid[[2]]-vector.grid[[1]]))
    mean.vector <- append(mean.vector, mean.one)
  }
  return(mean(mean.vector))
}

VarOfOverlappingNumberDiff <- function(cvConf, sim.count = 1000) {
  var.sample.vector <- c()
  for(i in 1:sim.count) {
    generator <- LoadCrossValidationGenerator(cvConf$name)
    cv.function <- generator[[1]]
    partition.set <- cv.function(cvConf)
    partition.matrix <- convertParitionSetToPartitionMatrix(partition.set)
    overlapping.numbers <- overlappingCountsOfPartitionSet(partition.matrix)
    overlapping.numbers <- overlapping.numbers[lower.tri(overlapping.numbers)]
    vector.grid <- expand.grid(overlapping.numbers,overlapping.numbers)
    vector.one <- abs(vector.grid[[2]]-vector.grid[[1]])
    var.sample.vector <- append(var.sample.vector, vector.one)
  }
  print(var.sample.vector)
  return(var(var.sample.vector))
}

CriticalValueInRegCondOfBRLT <- function (n, n1,J) {
  occur.sum.min <- J*n1
  occur.avg <- floor(J*n1/n)
  cv <- choose(occur.avg+1, 2)* (J*n1- occur.avg*n)+choose(occur.avg, 2)*((occur.avg+1) *n-J*n1)
  return (cv/choose(J,2))
}


MeanOfAbsDiffOfPhiWithCriticalValue <- function(n, n1, J, sim.count = 10000, setseed = FALSE) {
  if(setseed) {
    set.seed(12345)
  }
  rands <- rhyper(sim.count, n1, n-n1, n1)
  cv <- CriticalValueInRegCondOfBRLT(n, n1, J)
  rands.reform <- abs(rands-cv)
  return (mean(rands.reform))
}





set.seed(12345)

cvConf <- list(n=500, n1 = 250, m=5, name="rlt")
VarOfOverlappingNumberDiff(cvConf, sim.count=100)
cvConf <- list(n=500, n1=250, J=5, m=1, name="improvedRlt")
MeanOfOverlappingNumber(cvConf, sim.count = 100)
VarOfOverlappingNumberDiff(cvConf,sim.count = 100)