# 本程序为mx2bcv序贯t-检验的配置程序。
#
# 本程序的主要目的是识别mx2bcv序贯t-检验的配置是否正确。
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/11/1

source("datasets/data_loader.R", encoding="UTF-8")
source("mlAlgorithms/algor_loader.R", encoding="UTF-8")
source("crossvalidations/cv_loader.R", encoding="UTF-8")
source("varianceEstimator/var_est_loader.R", encoding = "UTF-8")
source("model_train_and_predict.R", encoding="UTF-8")
source("utils.R", encoding="UTF-8")
# 载入主函数
source("bmx2seqtest/mx2bcvSeqTest.R", encoding="UTF-8")
# 这里进行并行运算：
# 对于一个数据集,并行进行多种算法的比较。
#library(foreach)
#result = mx2bcvseqtest(dataset.conf,algorithm1.conf, algorithm2.conf,cvConf, conf_TASK_BMX2SEQTEST)


# 执行输出程序
# source("R/bmx2seqtest/output.R", encoding="UTF-8")