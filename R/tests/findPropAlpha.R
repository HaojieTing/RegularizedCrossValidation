# 找到合适的alpha, 使得第一类错误为0.05.

setwd("/share/home/wrb/rtest/bcv/R/")
source("tests/runitSeqTestOnWithEstFile.R", encoding="UTF-8")
m <- 12
data.tag <- "sreg1"
data.settings <- list(
  "sreg1" = list(
    mu.true = -0.0255,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/sreg.first_-1_-1_1000_20180618115602'
  ),
  "sreg2" = list(
    mu.true =  7.4647,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/sreg.second_-1_-1_1000_20180618115605'
  ),
  "sreg3" = list(
    mu.true =  0.0011,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/sreg.third_-1_-1_1000_20180618115611'
  ),
  "sreg4" = list(
    mu.true =  0.0398,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/sreg.fourth_-1_-1_1000_20180618115618'
  ),
  "scla1" = list(
    mu.true =  -0.0423,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/scla.first_-1_-1_1000_20180618115622'
  ),
  "scla2" = list(
    mu.true =  0.0012,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/scla.second_-1_-1_1000_20180618115628'
  ),
  "scla3" = list(
    mu.true =  -0.0043,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/scla.third_-1_-1_1000_20180618115630'
  ),
  "scla4" = list(
    mu.true =  0.0240,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/scla.fourth_-1_-1_1000_20180618115632'
  ),
  "letter1" = list(
    mu.true =  0.1561,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/letter.1_-1_-1_1000_20181107223023'
  ),
  "letter2" = list(
    mu.true =  0.1033,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/letter.2_-1_-1_1000_20181107223542'
  ),
  "letter3" = list(
    mu.true =  0.0644,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/letter.3_-1_-1_1000_20181107223548'
  ),
  "letter4" = list(
    mu.true =  0.0301,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/letter.4_-1_-1_1000_20181107223745'
  ),
  "letter5" = list(
    mu.true =  0.0065,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/letter.5_-1_-1_1000_20181107223333'
  ),
  "letter6" = list(
    mu.true =  -0.0814,
    file.name = '/share/home/wrb/rtest/bcv/R/est_for_test/letter.6_-1_-1_1000_20181107223629'
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
