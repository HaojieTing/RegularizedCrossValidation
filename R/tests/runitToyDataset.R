# 生成玩具数据集。

generateToyDataset <- function(data.tag.index, mu, rho.1, rho.2, sigma2) {
library(MASS)
rep.count <- 1000
mu <- mu
m <- 20
rho.2 <- rho.2
rho.1 <- rho.1
sigma2 <- sigma2
corr.matrix <- diag(2*m) 
corr.matrix[,] <- rho.2
for(m.index in 1:m) {
  for(i in 1:2) {
    corr.matrix[(m.index-1)*2+1,m.index*2] <- rho.1
    corr.matrix[m.index*2, (m.index-1)*2+1] <- rho.1
  }
}
diag(corr.matrix) <- 1.0
mu.vec <- rep(mu, 2*m)
cov.matrix <- sigma2 * corr.matrix # 生成协方差矩阵
data.set.diff <- mvrnorm(n = rep.count, mu = mu.vec, Sigma = cov.matrix) # 生成数据
data.set.A <- data.set.diff
data.set.B <- data.set.diff
data.set <- cbind(data.set.A, data.set.B, data.set.diff)
# 写出数据集
write.table(data.set, file = paste(paste("toy", data.tag.index, sep=""), -1, rep.count, format(Sys.time(), "%Y%m%d%H%M%S"),sep="_"), sep=" ")
}

generateToyDataset(2, 0, 0, 0, 1)
generateToyDataset(3, 0, 0, 0.1, 1)
generateToyDataset(4, 0, 0, 0.2, 1)
generateToyDataset(5, 0, 0, 0.3, 1)
generateToyDataset(6, 0, 0, 0.4, 1)
generateToyDataset(7, 0, 0, 0.5, 1)
