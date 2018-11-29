# 方差模拟任务的结果存储脚本
# 
# 具体要将中间结果和最终结果存储在文件中。
# 最终结果包括:
# 1. 总方差
# 2. 给定切分后，估计的方差，然后均值.
# 3. 给定切分后，估计的均值，然后方差.
# 中间结果包括:
# 1. 估计值矩阵.
# 2. 每个切分下，数据随机性引起的估计的方差.
# 3. 每个切分下，数据随机性引起的估计的均值.
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/5/21

line.1 <- paste("E(VAR(mu|partition)):", mean.estimator.vars)
line.2 <- paste("VAR(E(mu|partition)):", var.estimator.mean)
line.3 <- paste("Total var:", variance.total)

result.file.handler <- file(paste(data.dir, "result.txt", sep=.Platform$file.sep), "w")
writeLines(c(line.1, line.2, line.3), result.file.handler)
close(result.file.handler)

write.table(estimator.vars.conditioned.on.partitionset, file = paste(data.dir, "estimator.variance.on.partition", sep=.Platform$file.sep))
write.table(estimator.mean.conditioned.on.partitionset, file = paste(data.dir, "estimator.mean.on.partition", sep=.Platform$file.sep))
save(estimator.matrix, file=paste(data.dir, "estimator.matrix", sep=.Platform$file.sep))