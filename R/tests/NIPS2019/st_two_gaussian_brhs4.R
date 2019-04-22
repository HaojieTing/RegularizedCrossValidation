# 计算BRHS4在高斯数据集上的检验结果


brhs_task_config <- list(
  alpha = 0.05,
  delta = 0,
  var.est.conf = list(
   name= "var_est_brhs4"
  )
)

ge_file_path <- "D:/datasets/Zzzz/two_gaussian_sim2_brhs4_20190422213058"  # sim 2
ge_file_path <- "D:/datasets/Zzzz/two_gaussian_sim4_brhs4_20190422221249"  # sim 4

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