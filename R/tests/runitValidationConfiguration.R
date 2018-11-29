#
#
#
#
#
#

source("utils.R", encoding="UTF-8")
source("model_train_and_predict.R", encoding = "UTF-8")
source("crossvalidations/hold_out.R", encoding = "UTF-8")
source("crossvalidations/randommxkcv.R", encoding = "UTF-8")
source("mlAlgorithms/regression/LassoModel.R", encoding = "UTF-8")
source("datasets/regression/hdRegrDataFL.R", encoding = "UTF-8")

dataset.conf <- list(
  d =  500,
  type =  "regression",
  name =  "hdRegrDataFL",
  n =  500
)

testCheckVariableExist <- function() {
  exist <- CheckVariableExist("dataset.conf", "need to specify data set configurations!", error.stop = FALSE)
  checkTrue(exist)
}

testSwapObservantsToConstructNewPartition <- function () {
  crossvalidation.conf <- list(
    n=500, 
    n1=250
  )
  partition.first <- hold_out.Generator(crossvalidation.conf)
  count.exchange <- 10
  partition.second <- SwapObservantsToConstructNewPartition(partition.first, count.exchange)
  partition.second.len <- length(partition.second)
  checkTrue(partition.second.len == 2)
  crossvalidation.conf <- list(
    n=500, 
    n1=300
  )
  partition.first <- hold_out.Generator(crossvalidation.conf)
  count.exchange <- 10
  partition.second <- SwapObservantsToConstructNewPartition(partition.first, count.exchange)
  partition.second.len <- length(partition.second)
  checkTrue(partition.second.len == 2)
}


testTrainAndTestForHoldoutEstimator <- function() {
  crossvalidation.conf <- list(
    n=500, 
    n1=250
  )
  dataset.conf <- list(
    d =  500,
    type =  "regression",
    name =  "hdRegrDataFL",
    n =  500
  )
  algorithm.conf <- list(
    useGram =  TRUE,
    type =  "regression",
    name =  "LassoModel"
  )
  dataset.observants <- hdRegrDataFL.DataGenerator(dataset.conf)
  partition.first <- hold_out.Generator(crossvalidation.conf)
  pe.first  <- TrainAndTestForHoldoutEstimator(dataset.observants, partition.first, LassoModel.TrainAndTest, algorithm.conf, reverse = FALSE)
  checkTrue(length(pe.first) == 2)
  crossvalidation.conf <- list(
    n=500, 
    n1=300
  )
  library(MASS)
  library(lars)
  dataset.observants <- hdRegrDataFL.DataGenerator(dataset.conf)
  partition.first <- hold_out.Generator(crossvalidation.conf)
  pe.first  <- TrainAndTestForHoldoutEstimator(dataset.observants, partition.first, LassoModel.TrainAndTest, algorithm.conf, reverse = FALSE)
  checkTrue(length(pe.first) == 2)
}


testTrainAndTestForCrossValidationEstimator <- function() {
  crossvalidation.conf <- list(
    n = 500, 
    m = 3,
    v = 2
  )
  dataset.conf <- list(
    d =  500,
    type =  "regression",
    name =  "hdRegrDataFL",
    n =  500
  )
  algorithm.conf <- list(
    useGram =  TRUE,
    type =  "regression",
    name =  "LassoModel"
  )
  library(MASS)
  library(lars)
  dataset.observants <- hdRegrDataFL.DataGenerator(dataset.conf)
  partition.set <- randommxkcv.Generator(crossvalidation.conf)
  cv.est.res <- TrainAndTestForCrossValidationEstimatorWithPartitionSet(dataset.observants, partition.set, 
                                                                        LassoModel.TrainAndTest, algorithm.conf)
  print(cv.est.res)
  checkTrue(length(cv.est.res) == 2)
}



testResampleObservantsFromPopulationWithAdvancement <- function() {
  resample.config <- list(n=300, replace=TRUE, yBalance=TRUE)
  dataset <- ResampleObservantsFromPopulationWithAdvancement(iris, resample.config)
  checkTrue (nrow(dataset) == 300)
}
