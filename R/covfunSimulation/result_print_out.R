# 协方差模拟任务的结果存储脚本
# 
# 具体要将中间结果和最终结果存储在文件中。
# 要存储的中间结果包括：
# 1. 模拟出的holdout估计值。
# 2. 计算出的样本协方差矩阵。
# 3. 模拟出的损失函数矩阵。
# 要存储的最终结果包括:
# 1. 协方差值。
# 2. 方差值。
# 3. 相关系数。
# 4. 参数sigma^2, omega, gamma, tau。
# 5. 函数中x^2, x^1, x^0的系数。
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/5/9

# 存储计算中间结果
save(pe.holdout.matrix, file = paste(data.dir, "holdout.est.matrix", sep=.Platform$file.sep))
save(covariance.matrix, file=paste(data.dir, "covariance.matrix", sep=.Platform$file.sep))
save(loss.matrix.first, loss.matrix.second, file=paste(data.dir, "loss.matrix", sep=.Platform$file.sep))

# 写出关心的结果和指标
line.1 <- paste("covariance value", covariance.value, sep=":")
line.2 <- paste("variance value", variance.value, sep=":")
line.3 <- paste("correlation value", correlation.value, sep=":")
line.4 <- paste("sigma^2", parameter.sigma2, sep=":")
line.5 <- paste("omega", parameter.omega, sep=":")
line.6 <- paste("tau", parameter.tau, sep=":")
line.7 <- paste("gamma", parameter.gamma, sep=":")
line.8 <- paste("x^2's coeff", coeff.x2, sep=":")
line.9 <- paste("x^1's coeff", coeff.x1, sep=":")
line.10<- paste("x^0's coeff", coeff.x0, sep=":")

result.file.handler <- file(paste(data.dir, "result.txt", sep=.Platform$file.sep), "w")
writeLines(c(line.1, line.2, line.3, line.4, line.5, line.6, line.7, line.8, line.9, line.10), result.file.handler)
close(result.file.handler)