# 方差模拟的配置文件样例及释义
#
# 方差模拟实验所使用的配置文件，其中包含，任务类型定义、
# 数据集配置、机器学习算法配置、交叉验证配置、以及任务参数配置。
#
# Author: Ruibo WANG
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/5/21

taskVarianceSimulation  <- "variance simulation"  # 方差模拟任务常量

dataset.conf <- list(  # 数据集的配置选项
  type =  "regression",  # 类型为回归类型，具体值为datasets文件夹下的子文件名称。
  name =  "hdRegrDataFL",  # 数据集名称，具体值为datasets的子文件夹下，后缀为.R的文件名。
  phi =  0.5,  # 以下的phi, d, n, beta为具体数据的配置项，具体参见数据集对应的.R文件的注释。
  d =  500,
  n =  500,
  beta =  5
)

algorithm.conf <- list(  # 机器学习算法的配置项
  type =  "regression",  # 类型为回归类型，具体值为mlAlgorithms文件夹下的子文件名称。
  name =  "LassoModel",  # 算法名称，具体值为mlAlgorithms的子文件夹下，后缀为.R的文件名。
  useGram =  TRUE  # 机器学习算法的配置项，具体参见机器学习算法对应的.R文件的注释。
)

# 注意: 方差模拟任务中，交叉验证方法产生的切分数必须大于1个。
crossvalidation.conf <- list(  # 交叉验证方法的配置项
  name =  "randommxkcv",  # 交叉验证的方法名称，具体值为crossvalidations文件夹下，后缀为.R的文件名。
  v = 2,  # 以下的v, n, m为具体的交叉验证的配置项，具体参见交叉验证方法的.R文件的注释。
  n =  500,
  m = 3
)

task.conf <- list(
  type = taskVarianceSimulation,  # 任务类型为方差模拟任务，相应的变量在entry.R中定义。
  dataRepCount =  10000,  # 以下为方差模拟任务(covFunctionSimulation)的配置选项
  partitionSetRepCount =  1000,  # 具体的配置选项参见./varianceSimulation/config.R文件中的注释。
  partitionSetRndSeed = TRUE
)