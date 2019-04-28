# 计算BRHS4在高斯数据集上的检验结果

brhs_task_config <- list(
  alpha = 0.05,
  delta = 0,
  var.est.conf = list(
   name= "var_est_brhs4"
  )
)

ge_file_path_brhs <- "D:/datasets/Zzzz/letter_brhs4_20190423111053"
ge_file_path_b3x2cv <- "D:/datasets/Zzzz/letter_3x2bcv_20190423110553"

mu.data <- read.table(ge_file_path)

source("./signif_Test/brhs4_t_test.R", encoding = "UTF-8")
record.count <- nrow(mu.data)
decision <- c()
for(i in 1:record.count) {
  if(i%%100==0) cat(paste(i,"..."))
  mu.vec <- c(as.numeric(mu.data[i,]))
  brhs_task_config$mu.vec <- mu.vec
  brhs_task_config <- brhs4_t_test.task_config_validation(brhs_task_config)
  decision <- c(decision, brhs4_t_test.perform_task(brhs_task_config))
}
print(decision)