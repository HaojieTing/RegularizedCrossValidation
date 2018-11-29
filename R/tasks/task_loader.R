# 加载并执行各种任务，获得每种任务的输出。
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/11/17


LoadTaskInfo <- function(task_config) {
  # 根据任务配置，加载该任务的相应执行函数
  # 相应的函数含三个:
  #   1. 验证函数
  #   2. 执行函数
  #   3. 输出结果函数
  
}

PerformTask <- function(task_config) {
  # 根据任务配置，运行相应的任务。
  # 任务列表见本文件夹下的各文件名。
  # Args:
  #   task_config: 任务配置
  # Return:
  #   Null
  result <- loadTaskInfo(task_config)
  task.validation <- result[[1]]
  task.perform <- result[[2]]
  task.write_result <- result[[3]]
  task_config <- task.validation(task_config)
  output_result <- task.perform(task_config)
  task.write_result <- task.write_result(output_result, task_config)
}