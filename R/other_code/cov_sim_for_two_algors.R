setwd(paste(getwd(), "other_code", sep = "/"))
source("../datasets/data_loader.R", encoding="UTF-8")
source("../mlAlgorithms/algor_loader.R", encoding="UTF-8")
source("../crossvalidations/cv_loader.R", encoding="UTF-8")
source("../model_train_and_predict.R", encoding="UTF-8")
source("../utils.R", encoding="UTF-8")

#total_repetition = rpt * split_group_number
data.size <- 1200
rep.count <- 100

dataConf <- list(name="regrDataBG_nooutliers",type="regression",src="",
                 cnt=0,n = data.size,d=500)
DataInfo <- LoadDataSetGenerator(dataConf)
DataGenerator <- DataInfo[[1]]  # 用来生成数据的函数
DataPackages <- DataInfo[[2]]  # 用来生成数据所需的R包

algorConf <- list(name="lmRidgeModel",type="regression", lambda=0)
AlgorInfo <- LoadAlgorithmGenerator(algorConf)
AlgorGenerator <- AlgorInfo[[1]]
AlgorPackages <- AlgorInfo[[2]]
# install.packages("tree")

algorConf2 <- list(name="linearModel", type="regression",no_intercept=F)
AlgorInfo2 <- LoadAlgorithmGenerator(algorConf2)
AlgorGenerator2 <- AlgorInfo2[[1]]
AlgorPackages2 <- AlgorInfo2[[2]]

cvConf <- list(name="increaseBalancedMx2cv", m = 2, data=NULL)
if(cvConf$name != 'increaseBalancedMx2cv') {
  stop("type of cross validation needed increaseBalancedMx2cv");
}
CrossValidationInfo <- LoadCrossValidationGenerator(cvConf$name)
CrossValidationGenerator <- CrossValidationInfo[[1]]
CrossValidationPackages <- CrossValidationInfo[[2]]

library("MASS")
data <- DataGenerator(dataConf)
cvConf$data <- data
cvConf <- CrossValidationGenerator(cvConf)
split <- cvConf$splits_new
muv1 <- c(); muv2 <- c(); muv3 <- c(); muv4 <- c(); muv5 <- c(); muv6 <- c(); muv7 <- c(); muv8 <- c()


for(i in 1:rep.count){
  if(i %% 10 == 0) print(paste(i ," repititions"))
  dataConf <- list(name="regrDataBG_nooutliers",type="regression",n=data.size, d=500)
  data <- DataGenerator(dataConf)
  cvres1 <- TrainAndTestForCrossValidationEstimatorWithPartitionSet(data, split, AlgorGenerator, algorConf,
                                                                    NULL, NULL)
  cvres2 <- TrainAndTestForCrossValidationEstimatorWithPartitionSet(data, split, AlgorGenerator2, algorConf2,
                                                                    NULL, NULL)
  muv1 <- c(muv1, cvres1[[2]][1])
  muv2 <- c(muv2, cvres1[[2]][2])      
  muv3 <- c(muv3, cvres1[[2]][3])
  muv4 <- c(muv4, cvres1[[2]][4]) 
  muv5 <- c(muv5, cvres2[[2]][1])
  muv6 <- c(muv6, cvres2[[2]][2])
  muv7 <- c(muv7, cvres2[[2]][3])
  muv8 <- c(muv8, cvres2[[2]][4])
}

muvdiff1 <- muv1 - muv5
muvdiff2 <- muv2 - muv6
muvdiff3 <- muv3 - muv7
muvdiff4 <- muv4 - muv8

write.csv(muvdiff1, "(n=20000)muvdiff1.csv",row.names=F)
write.csv(muvdiff2, "(n=20000)muvdiff2.csv",row.names=F)
write.csv(muvdiff3, "(n=20000)muvdiff3.csv",row.names=F)
write.csv(muvdiff4, "(n=20000)muvdiff4.csv",row.names=F)

a1 <- var(muvdiff1); a2 <- var(muvdiff2); a3 <- var(muvdiff3); a4 <- var(muvdiff4)  #方差
b1 <- cov(muvdiff1,muvdiff2); b2 <- cov(muvdiff3,muvdiff4)  #0协方差
c1 <- cov(muvdiff1,muvdiff3); c2 <- cov(muvdiff1,muvdiff4); c3 <- cov(muvdiff2,muvdiff3); c4 <- cov(muvdiff2,muvdiff4)  #n/4协方差
a <- mean(c(a1,a2,a3,a4))
b <- mean(c(b1,b2))
c <- mean(c(c1,c2,c3,c4))
a;b;c
rho1 <- b/a; rho2 <- c/a
rho1;rho2
