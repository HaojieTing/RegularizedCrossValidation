# 找到合适的alpha, 使得第一类错误为0.05.

setwd("/share/home/wrb/rtest/bcv/R/")
source("tests/runitSeqTestOnWithEstFile.R", encoding="UTF-8")
m <- 12
data.tag <- "toy_0_0"
data.settings <- list(
  "toy_0_0" = list(
    mu.true = 0,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/toy2_-1_1000_20181120101406'
  ),
  "toy_0_2" = list(
    mu.true = 0,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/toy4_-1_1000_20181120110333'
  ),
  "toy_0_4" = list(
    mu.true = 0,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/toy6_-1_1000_20181120110335'
  ),
  "toy_0_5" = list(
    mu.true = 0,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/toy7_-1_1000_20181120110337'
  ),
  
  "toy_2_0" = list(
    mu.true = 0,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/toy2_0_-1_1000_20181125142647'
  ),
  "toy_2_2" = list(
    mu.true = 0,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/toy2_2_-1_1000_20181125142904'
  ),
  "toy_2_4" = list(
    mu.true = 0,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/toy2_4_-1_1000_20181125143028'
  ),
  "toy_2_5" = list(
    mu.true = 0,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/toy2_5_-1_1000_20181125143123'
  ),
  
  "toy_4_0" = list(
    mu.true = 0,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/toy4_0_-1_1000_20181125142704'
  ),
  "toy_4_2" = list(
    mu.true = 0,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/toy4_2_-1_1000_20181125142917'
  ),
  "toy_4_4" = list(
    mu.true = 0,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/toy4_4_-1_1000_20181125143052'
  ),
  "toy_4_5" = list(
    mu.true = 0,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/toy4_5_-1_1000_20181125143136'
  ),
  
  "toy_5_0" = list(
    mu.true = 0,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/toy5_0_-1_1000_20181125142711'
  ),
  "toy_5_2" = list(
    mu.true = 0,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/toy5_2_-1_1000_20181125142923'
  ),
  "toy_5_4" = list(
    mu.true = 0,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/toy5_4_-1_1000_20181125143059'
  ),
  "toy_5_5" = list(
    mu.true = 0,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/toy5_5_-1_1000_20181125143142'
  )
)

mu.true <- data.settings[[data.tag]][["mu.true"]]
file_name <- data.settings[[data.tag]][["file.name"]]
alphas <- seq(0.01, 0.2, 0.01)
sim.count <- length(alphas)
library(foreach)
library(doParallel)
cl <- makeCluster(type="MPI")
registerDoParallel(cl)
results <- foreach(i=1:sim.count, .combine = rbind, .export = ls(.GlobalEnv)) %dopar% {
  setwd("/share/home/wrb/rtest/bcv/R/")
  source("tests/runitSeqTestOnWithEstFile.R", encoding="UTF-8")
  alpha.value <- alphas[i]
  result_diet <- testMx2BCVSeqTest_dietterich(m, mu.true, file.name=file_name, alpha= alpha.value)
  result_bmx2 <- testMx2BCVSeqTest_bmx2cv(m, mu.true, file.name=file_name, est.type="typeIV", alpha= alpha.value)
  c(alpha.value, result_diet[length(result_diet)], result_bmx2[length(result_bmx2)])
}
stopCluster(cl)
write.csv(results, file = paste("est_for_test/", paste("find_alpha_", data.tag, format(Sys.time(), "%Y%m%d%H%M%S"),sep="_"), sep=""))
