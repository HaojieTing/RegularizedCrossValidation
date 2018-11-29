# 测试统计数据集的基本信息函数是否正确
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/11/10
source("utils.R", encoding="UTF-8")
datasets.names <- c("artificial","flare","magic","australian","german","mammographic",
                    "balance","glass","monk","bupa","haberman","nursery","car","hayesroth",
                    "optdigits","cmc","heart","page_block","credit","hepatitis","parkinsons",
                    "cylinder","horse","pendigits","dermatology","iris","pima","donors",
                    "ironosphere","post_operative","ecoli","krvskp","promoters","flags",
                    "letter","satellite47","spambase","spect","tae","tic_tac_toe","titanic",
                    "transfusion","vehicle","vote","wave","wine","yeast","zoo")

source("./datasets/data_loader.R", encoding = "UTF-8")

testStateBasicInfoOfDataSets <- function() {
  results <- c()
  for(dataset.name in datasets.names) {
    dataset.conf <- list(
      name = paste("uci", dataset.name, sep="_"),
      type = "classification"
    )
    result <- StatBasicInfoOfDataSet(dataset.conf)
    results <- rbind(results, result)
    cat(dataset.name, "\n")
    print(result)
    warnings()
  }
  print(results)
}

