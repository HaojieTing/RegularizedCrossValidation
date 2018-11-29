source("./header.R")
library(optparse)
library(digest)

option_list <- list( 
  make_option(c("-n","--n"), default=200, help="观测集大小[默认值 %default]"),
  make_option("--ntrain",default=100, help="训练集的大小[默认值 %default]"),
  make_option(c("-d","--dim"), default=3, help="观测集数据维度[默认值 %default]"),
  make_option(c("-r","--rep"), default=1000,help="数据的模拟次数"),
  make_option("--sptrpt", default=25, help="切分的模拟次数[默认值 %default]"), 
  make_option("--alpha", default=0.05,help="分位数"),
  make_option("--datatype", default="regrDataNB_nooutliers", help="数据类型"), #确定了数据类型，就确定了数据的产生函数
  make_option("--machAlgor1", help="机器学习算法1"),
  make_option("--machAlgor2", help="机器学习算法2"),
  make_option("--machLearnType", default="regression", help="机器学习算法类型")
)

opt <- parse_args(OptionParser(option_list=option_list))

t_algorithm1 <- opt$machAlgor1
if(is.null(t_algorithm1))
  stop("没有指定要模拟的算法")
t_alpha <- opt$alpha
t_n <- opt$n
t_ntrain <- opt$ntrain
t_d <- opt$dim
t_rpt <- opt$rep
t_sptrpt <- opt$sptrpt    #切分次数
t_datatype <- opt$datatype
t_algorithm2 <- opt$machAlgor2
t_mltype <- opt$machLearnType

exist <- FALSE
sname = digest(list("trueci",t_algorithm1,t_alpha,t_n,t_ntrain,t_d,t_rpt,t_sptrpt,t_datatype,t_algorithm2,t_mltype),algo="md5")
filename  <- paste("../trueCIData/",sname,".RData",sep="")
count <- 1
while(file.exists(filename)){
  load(filename)
  if(
    t_algorithm1 == algorithm1 && t_alpha == alpha && t_n == n && t_ntrain == ntrain &&
      t_d == d && t_rpt == rpt && 
      t_sptrpt == sptrpt &&
      t_datatype == datatype &&  t_algorithm2 == algorithm2 && t_mltype == mltype     
  )#全部相等，则退出
  {    
    exist <- TRUE 
    break
  }
  filename <-  paste("../trueCIData/",count,".",sname,".RData",sep="")
  count <- count + 1
}
if(exist){
  source("./resultOut.R")
}else{
  algorithm1 = t_algorithm1 
  alpha = t_alpha 
  n = t_n 
  ntrain = t_ntrain 
  d = t_d 
  rpt = t_rpt 
  sptrpt = t_sptrpt
  datatype = t_datatype   
  datagenfun <- datatype
  algorithm2 = t_algorithm2 
  mltype = t_mltype  
  source("./dataTrueCISimulation.R")
  source("./resultOut.R")
}
source("../quit.R")