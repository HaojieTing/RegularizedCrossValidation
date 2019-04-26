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
    
  ),
  iris = list(
    name = "uci_iris",
    type = "classification"
  ),
  vowel = list(
    name = "uci_vowel",
    type = "classification"
  ),
  seed = list(
    name = "uci_seed",
    type = "classification"
  ),
  
  balance = list(
    name = "uci_balance",
    type = "classification"
  )
)