# 计算uci5个数据上的第一类错误和第二类错误。
source("./signif_Test/brhs4_t_test.R", encoding="UTF-8")
source("./signif_Test/mx2bcv_t_test.R", encoding="UTF-8")

perform_brhs_t_test <- function(file.name) {
  data.set <- read.table(file.name)
  for(i in 1:nrow(data.set)){
    brhs_task_config <- list(
      alpha = 0.05,
      delta = 0,
      var.est.conf = list(
        name= "var_est_brhs4"
      )
    )
    mu.vec <- c(as.numeric(data.set[i,]))
    brhs_task_config$mu.vec <- mu.vec
    brhs_task_config <- brhs4_t_test.task_config_validation(brhs_task_config)
    decision <- c(decision, brhs4_t_test.perform_task(brhs_task_config))  
  }
  return(mean(decision))
}

perform_b3x2_t_test <- function(file.name) {
  data.set <- read.table(file.name)
  for(i in 1:nrow(data.set)){
    b3x2cv_task_config <- list(
      alpha = 0.05,
      delta = 0,
      var.est.conf = list(
        name = "var_est3_mx2cv",
        m = 3
      )
    )
    mu.vec <- c(as.numeric(data.set[i,]))
    b3x2cv_task_config$mu.vec <- mu.vec
    b3x2cv_task_config <- mx2bcv_t_test.task_config_validation(b3x2cv_task_config)
    decision <- c(decision, mx2bcv_t_test.perform_task(b3x2cv_task_config))
  }
  return(mean(decision))
}


uci_five_datasets <- c("iris", "vowel", "seed", "heart", "balance")
counts.hidden <- c(3,5,10,20)
file.names <- list.files("D:/datasets/Zzzz/")
for(data.tag in uci_five_datasets) {
  for(count.hidden in counts.hidden) {
    for(sgl in c("TRUE", "FALSE")) {
      for(test_method in c("3x2bcv", "brhs4")) {
        str.prefix <- paste("uci", data.tag, count.hidden, sgl, test_method, sep = "_")
        finded.filename <- NULL
        for(filename in file.names) {
          filename.length <- nchar(filename)
          cur.prefix <-  substr(filename, start = 0, stop = filename.length-15)
          if(str.prefix == cur.prefix) {
            finded.filename <- filename
          }
        }
        if(!is.null(finded.filename)) {
          file.abs.name <- file.path("D:/datasets/Zzzz/", finded.filename)
          print(str.prefix)
          if(test_method == "3x2bcv") {
            perform_b3x2_t_test(file.abs.name)
          } else {
            perform_brhs_t_test(file.abs.name)
          }
        }
      } 
    }
  }
}