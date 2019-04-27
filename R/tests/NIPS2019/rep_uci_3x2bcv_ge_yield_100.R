source("datasets/data_loader.R", encoding="UTF-8")
source("mlAlgorithms/algor_loader.R", encoding="UTF-8")
source("crossvalidations/cv_loader.R", encoding="UTF-8")
source("varianceEstimator/var_est_loader.R", encoding = "UTF-8")
source("model_train_and_predict.R", encoding="UTF-8")
source("utils.R", encoding="UTF-8")
source("metrics/metric_loader.R", encoding="UTF-8")

datasets.configs <- list(
  balance = list(
    name = "uci_balance",
    type = "classification"
  ),
  diabetes = list(
    name = "uci_diabetes",
    type = "classification"
  ),
  glass = list(
    name = "uci_glass",
    type = "classification"
  ),
  heart = list(
    name = "uci_heart",
    type = "classification"
  ),
  ionosphere = list(
    name = "uci_ionosphere",
    type = "classification"
  ),
  iris = list(
    name = "uci_iris",
    type = "classification"
  ),
  vehicle = list(
    name = "uci_vehicle",
    type = "classification"
  ),
  wine = list(
    name = "uci_wine",
    type = "classification"
  ),
  yeast = list(
    name = "uci_yeast",
    type = "classification"
  ),
  seed = list(
    name = "uci_seed",
    type = "classification"
  )
)


task_config <- list(
  # 数据集配置根据用户参数进行自动填充。
  algorithm3.conf = list(
    name = "regressionTree",
    type = "classification",
    test_method = "class"
  ),
  algorithm2.conf = list(
    name = "LinearDiscriminantAnalysis",
    type = "classification"
  ),
  algorithm1.conf = list (
    name = "naiveBayes",
    type = "classification"
  ),
  crossvalidation.conf = list(
    name = "rhsbcv",
    J  = 4
  )
)