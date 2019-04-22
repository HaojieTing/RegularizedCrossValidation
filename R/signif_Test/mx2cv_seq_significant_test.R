# 对比两个机器学习算法的序贯检验。
#
# 检验问题为：
#     H0: \mu < \Delta_0  vs   H1: \mu > \Delta_1
#
# 支持如下具体的检验:
#   - 基于mx2 bcv的序贯检验
#
# Author: Wang Ruibo
# E-mail: wangruibo@sxu.edu.cn
# Date: 2017/11/18

mx2cv_seq_significant_test.compute_f_m <- function(m, rho1, rho2) {
  nom <- 2*m*(1-rho2)-(1+rho1-2*rho2)
  nom <- nom * nom
  den <- m*(1-rho1)^2+(m-1)*(1+rho1-2*rho2)^2
  return(nom /den)
}

mx2cv_seq_significant_test.compute_c_m <- function(m, rho1, rho2) {
  nom <- 1+rho1 + 2.0*(m-1)*rho2
  den <- 2.0*m-nom
  return(sqrt(nom /den))
}

mx2cv_seq_significant_test.compute_c_m_true <- function(m, task_config) {
  rho1 <- task_config$rho1
  rho2 <- task_config$rho2
  v <- mx2cv_seq_significant_test.compute_c_m(m, rho1, rho2)
  return(v)
}

mx2cv_seq_significant_test.compute_f_m_true <- function(m, task_config){
  rho1 <- task_config$rho1
  rho2 <- task_config$rho2
  v <- mx2cv_seq_significant_test.compute_f_m(m, rho1, rho2)
  return(v)
}

mx2cv_seq_significant_test.compute_c_m_hat14 <- function(m, task_config) {
  return(1)
}

mx2cv_seq_significant_test.compute_f_m_hat14 <- function(m , task_config) {
  return(2*m-1)
}

mx2cv_seq_significant_test.compute_c_m_hat_org <- function(m, task_config) {
  term1 <- 2.0*m+1
  term2 <- 2.0*m-1
  term <- sqrt(term1/term2)
  return(term)
}

mx2cv_seq_significant_test.compute_c_m_hat <- function(m, task_config) {
  # 计算序贯检验时所用到的C_m的估计量。
  # 
  # Args:
  #   m: 组块mx2中的重复次数m
  # Return:
  #   C_m的估计
  term1 <- sqrt(2*m-1)-m
  term2 <- 2*m*(asin(1/sqrt(2))-asin(1/sqrt(2*m)))
  est <- 1/(m-1)*(term1+term2)
  return(est)
}

mx2cv_seq_significant_test.compute_c_m_hat_as_one <- function(m, task_config) {
  return(1.0)
}

mx2cv_seq_significant_test.compute_c_m_hat_improve_org <- function(m, task_config) {
  term1 <- 2.0*m+1
  term2 <- 2.0*m-1
  term <- sqrt(term1/term2)
  return(1.0/sqrt(2)*term)
}

mx2cv_seq_significant_test.compute_c_m_hat_improve2_org <- function(m, task_config) {
  term1 <- 2.0*m+1
  term2 <- 2.0*m-1
  term <- sqrt(term1/term2)
  return(0.72*term)
}

mx2cv_seq_significant_test.compute_c_m_hat_improve3_org <- function(m, task_config) {
  term1 <- 2.0*m+1
  term2 <- 2.0*m-1
  term <- sqrt(term1/term2)
  return(0.8*term)
}

mx2cv_seq_significant_test.compute_c_m_hat_improve4_org <- function(m, task_config) {
  term1 <- 2.0*m+1
  term2 <- 2.0*m-1
  term <- sqrt(term1/term2)
  return(0.9*term)
}

mx2cv_seq_significant_test.compute_c_m_hat_improve5_org <- function(m, task_config) {
  rho1 <- 0.25
  rho2 <- 0.4
  nom <- 1+rho1 + 2.0*(m-1)*rho2
  den <- 2.0*m-nom
  return(sqrt(nom /den))
}

mx2cv_seq_significant_test.compute_c_m_hat_improve6_org <- function(m, task_config) {
  rho1 <- 0.3
  rho2 <- 0.4
  nom <- 1+rho1 + 2.0*(m-1)*rho2
  den <- 2.0*m-nom
  return(sqrt(nom /den))
}

mx2cv_seq_significant_test.compute_c_m_hat_improve7_org <- function(m, task_config) {
  rho1 <- 0.4
  rho2 <- 0.4
  nom <- 1+rho1 + 2.0*(m-1)*rho2
  den <- 2.0*m-nom
  return(sqrt(nom /den))
}

mx2cv_seq_significant_test.compute_c_m_hat_improve8_org <- function(m, task_config) {
  rho1 <- 0.0
  rho2 <- 0.5
  nom <- 1+rho1 + 2.0*(m-1)*rho2
  den <- 2.0*m-nom
  return(sqrt(nom /den))
}

mx2cv_seq_significant_test.compute_f_m_hat_improve8_org <- function(m, task_config) {
  rho1 <- 0.0
  rho2 <- 0.5
  nom <- (2*m*(1-rho2)-(1+rho1-2*rho2))^2
  dom <- m*(1-rho1)^2+(m-1)*(1+rho1-2*rho2)^2
  return(nom/dom)
}

mx2cv_seq_significant_test.compute_c_m_hat_another <- function(m, task_config) {
  term1 <- 2.0*m/(2.0*m-1)
  term2 <- asin(1.0/(2.0*m)) - asin((1.0-m)/m) - cos(asin(1.0/(2.0*m))) + cos(asin((1.0-m)/m))
  return(term1*term2)
}

mx2cv_seq_significant_test.compute_c_m_hat_empirical <- function(m, task_config) {
  # rho1从0到rho2, rho2从0到1/2
  cm_hats <- c(0.928064843,0.853074014,0.817008224,0.795686947,0.781569509,0.771520224,0.763996666,0.758150253,0.753474976,0.749650141,0.746462541,0.743764849,0.741451971,0.739446905,0.737691935,0.736142945,0.734765627,0.733532902,0.732423106,0.731418702,0.73050534,0.729671164,0.728906294,0.728202428,0.72755254,0.726950645,0.72639161,0.725871013,0.725385015,0.724930278,0.724503875,0.724103237,0.723726096,0.723370442,0.723034487,0.72271664,0.722415472,0.722129704,0.721858184)
  if(m <= 1) stop("m is too small")
  cm_hats.length <- length(cm_hats)
  if (m > 1+cm_hats.length) stop("m is too large")
  return(cm_hats[m-1])
}

mx2cv_seq_significant_test.compute_c_m_hat_empirical_improved <- function(m, task_config) {
  # rho1从1/6到rho2, rho2从1/6到1/2
  cm_hats <- c(1.03316159751,0.950966441224,0.912147982981,0.889464872792,0.874570420421,0.86403522087,0.856187601921,0.850114496553,0.845274674769,0.84132683746,0.83804505466,0.835273793916,0.83290247165,0.830850303594,0.829056897401,0.82747619842,0.82607247207,0.824817565797,0.823688999015,0.822668602877,0.821741533655,0.820895545214,0.820120444531,0.81940767867,0.818750017628,0.818141308041,0.817576279945,0.817050393722,0.816559717804,0.816100830155,0.815670738307,0.815266814007,0.814886739431,0.814528462667,0.81419016064,0.813870208074,0.813567151374,0.813279686539,0.813006640399)
  if(m <= 1) stop("m is too small")
  cm_hats.length <- length(cm_hats)
  if (m > 1+cm_hats.length) stop("m is too large")
  return(cm_hats[m-1])
}

mx2cv_seq_significant_test.compute_c_m_hat_empirical_improved_v2 <- function(m, task_config) {
  # rho1从0到rho2, rho2从1/6到1/2
  cm_hats <- c(0.960013449636,0.889392362431,0.855675160704,0.835858917812,0.822799522114,0.813538980366,0.806628023099,0.801272137427,0.796999046778,0.79351027548,0.790607887279,0.788155418585,0.786055722395,0.784237754315,0.782648354149,0.781246949632,0.780002037612,0.778888783425,0.777887344671,0.776981676509,0.776158664441,0.775407484415,0.774719123617,0.774086016762,0.773501766662,0.772960927123,0.772458832539,0.771991462858,0.771555335653,0.771147419139,0.770765061562,0.770405933451,0.770067980099,0.769749382207,0.769448523108,0.769163961315,0.768894407419,0.768638704547,0.76839581176)
  if(m <= 1) stop("m is too small")
  cm_hats.length <- length(cm_hats)
  if (m > 1+cm_hats.length) stop("m is too large")
  return(cm_hats[m-1])
}

mx2cv_seq_significant_test.compute_c_m_hat_empirical_improved_v3 <- function(m, task_config) {
  # rho1从0到rho2, rho2从1/4到1/2
  cm_hats <- c(0.991824814329,0.925179942465,0.893467973583,0.874877074032,0.862648197756,0.853989076861,0.847534293758,0.842536530344,0.838552178567,0.835301211598,0.832598115856,0.830315104768,0.828361278356,0.826670205793,0.82519220898,0.823889393601,0.822732350073,0.821697904262,0.820767547836,0.819926320106,0.81916199677,0.818464491564,0.817825408356,0.817237701326,0.816695413976,0.816193476425,0.815727546345,0.815293882965,0.814889246374,0.814510816411,0.81415612681,0.813823011375,0.813509559689,0.813214080441,0.812935070887,0.812671191285,0.812421243373,0.812184152173,0.811958950534)
  if(m <= 1) stop("m is too small")
  cm_hats.length <- length(cm_hats)
  if (m > 1+cm_hats.length) stop("m is too large")
  return(cm_hats[m-1])
}

mx2cv_seq_significant_test.compute_f_m_hat_empirical <- function(m, task_config) {
  # rho1从0到rho2, rho2从0到1/2
  fm_hats <- c(2.8029237064,4.61440130639,6.42765692269,8.24157910743,10.0558232827,11.8702475104,13.684782602,15.4993907972,17.3140497374,19.1287453389,20.9434682887,22.7582121822,24.5729724698,26.3877458308,28.2025297846,30.0173224408,31.8321223338,33.6469283095,35.4617394466,37.2765550013,39.0913743659,40.9061970394,42.7210226049,44.5358507127,46.3506810674,48.1655134175,49.9803475479,51.7951832731,53.6100204327,55.424858887,57.2396985139,59.0545392062,60.8693808693,62.6842234197,64.499066783,66.3139108929,68.1287556903,69.9436011219,71.75844714)
  if(m <= 1) stop("m is too small")
  fm_hats.length <- length(fm_hats)
  if (m > 1+fm_hats.length) stop("m is too large")
  return(fm_hats[m-1])
}

mx2cv_seq_significant_test.compute_f_m_hat_empirical_improved <- function(m, task_config) {
  # rho1从1/6到rho2, rho2从1/6到1/2
  fm_hats <- c(2.88613089194,4.78024101539,6.67595622286,8.57226611701,10.4688614604,12.3656157825,14.2624677198,16.1593838927,18.0563445844,19.9533373991,21.8503541523,23.7473892225,25.6444386204,27.5414994371,29.4385695011,31.3356471586,33.2327311281,35.1298204012,37.0269141732,38.9240117946,40.821112735,42.7182165572,44.6153228977,46.5124314516,48.4095419612,50.3066542074,52.2037680027,54.1008831855,55.9979996162,57.8951171729,59.7922357495,61.6893552526,63.5864755999,65.4835967185,67.3807185439,69.2778410183,71.1749640901,73.0720877133,74.9692118462)
  if(m <= 1) stop("m is too small")
  fm_hats.length <- length(fm_hats)
  if (m > 1+fm_hats.length) stop("m is too large")
  return(fm_hats[m-1])
}

mx2cv_seq_significant_test.compute_f_m_hat_empirical_improved_v2 <- function(m, task_config) {
  # rho1从0到rho2, rho2从1/6到1/2
  fm_hats <- c(2.779095952,4.567583823,6.35803357947,8.14921990936,9.94076230707,11.7325039019,13.5243681855,15.316313388,17.1083147696,18.9003567439,20.6924290024,22.4845244548,24.2766380642,26.0687661536,27.8609059762,29.6530554385,31.4452129174,33.2373771345,35.0295470697,36.8217218989,38.613900949,40.4060836651,42.1982695853,43.9904583224,45.7826495489,47.5748429864,49.3670383964,51.1592355735,52.95143434,54.743634541,56.5358360414,58.3280387224,60.1202424792,61.9124472191,63.7046528598,65.4968593279,67.2890665577,69.0812744904,70.873483073)
  if(m <= 1) stop("m is too small")
  fm_hats.length <- length(fm_hats)
  if (m > 1+fm_hats.length) stop("m is too large")
  return(fm_hats[m-1])
}

mx2cv_seq_significant_test.compute_f_m_hat_empirical_improved_v3 <- function(m, task_config) {
  # rho1从0到rho2, rho2从1/4到1/2
  fm_hats <- c(2.74868393595,4.50732662642,6.26807636713,8.02962045994,9.79154937346,11.5536938546,13.3159712276,15.0783363066,16.8407623074,18.6032323452,20.3657352472,22.1282633259,23.8908111181,25.6533746345,27.4159508944,29.1785376257,30.941133066,32.7037358276,34.4663448025,36.2289590959,37.9915779767,39.7542008419,41.5168271899,43.2794565998,45.042088716,46.8047232358,48.5673599001,50.3299984857,52.0926387993,53.8552806729,55.6179239593,57.3805685294,59.1432142695,60.9058610786,62.6685088674,64.4311575559,66.1938070728,67.9564573542,69.7191083424)
  if(m <= 1) stop("m is too small")
  fm_hats.length <- length(fm_hats)
  if (m > 1+fm_hats.length) stop("m is too large")
  return(fm_hats[m-1])
}


mx2cv_seq_significant_test.compute_f_m_hat <- function(m, task_config) {
  #return(2/3*(2*m-1))
  return(1.5*m-0.5)
}

mx2cv_seq_significant_test.compute_f_m_hat_conv <- function(m, task_config){
  return(m)
}

mx2cv_seq_significant_test.compute_f_m_hat_v2 <- function(m, task_config) {
  return(11/15*(2*m-1))
}

mx2cv_seq_significant_test.compute_f_m_hat_v3 <- function(m, task_config) {
  return(23/36*(2*m-1))
}

mx2cv_seq_significant_test.compute_f_m_hat_v4 <- function(m, task_config) {
  return(1/2.0*(2*m-1))
}


mx2cv_seq_significant_test.est_types <- list(
  "typeTrue" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_true,
    "fm" = mx2cv_seq_significant_test.compute_f_m_true
  ),
  "type14est" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat14,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat14
  ),
  "typeI" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat
  ),
  "typeII" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat14
  ),
  "typeIII" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat14,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat
  ),
  "typeIV" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_org,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat14
  ),
  "typeV" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_org,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat
  ),
  "typeVI" = list (
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_as_one,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat
  ),
  "typeVII" = list (
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_improve_org,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat
  ),
  "typeVIII" = list (
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_improve2_org,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat
  ),
  "typeIX" = list (
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_improve3_org,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat
  ),
  "typeX" = list (
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_empirical,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat
  ),
  "typeXI" = list (
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_improve4_org,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat
  ),
  "typeXII" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_empirical,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat_empirical
  ),
  "type13" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_another,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat
  ),
  "type14" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_improve5_org,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat
  ),
  "type15" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_improve6_org,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat
  ),
  "type16" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_improve7_org,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat
  ),
  "type17" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_empirical_improved,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat_empirical_improved
  ),
  "type18" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_empirical_improved,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat
  ),
  "type19" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_empirical_improved,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat_v2
  ),
  "type20" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_empirical_improved_v2,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat_empirical_improved_v2
  ),
  "type21" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_empirical_improved_v2,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat
  ),
  "type22" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_empirical_improved_v2,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat_v3
  ),
  "type23" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_empirical_improved_v3,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat_empirical_improved_v3
  ),
  "type24" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_improve8_org,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat_improve8_org
  ),
  "type25" = list(
    "cm" = mx2cv_seq_significant_test.compute_c_m_hat_org,
    "fm" = mx2cv_seq_significant_test.compute_f_m_hat_conv
  )
)


mx2cv_seq_significant_test.task_config_validation <- function(task_config) {
  # 验证序贯检验任务的配置是否正确.
  #
  # Args:
  #   task_config: 序贯检验的任务配置
  # Return:
  #   更新后的任务配置
  source("varianceEstimator/var_est_loader.R", encoding = "UTF-8")
  source("model_train_and_predict.R", encoding="UTF-8")
  source("utils.R", encoding="UTF-8")
  source("metrics/metric_loader.R", encoding="UTF-8")
  # 第一类错误概率
  if(is.null(task_config$alpha)) stop("The type I prob is not provided")  
  # 原假设中的delta
  if(is.null(task_config$delta)) stop("The delta in H0 is not provided")
  # m参数停止的上界
  if(is.null(task_config$upper_m))  stop("The upper_m in H1 is not provided")
  # 判决算法的相对误差
  if(is.null(task_config$relative)) task_config$relative <- F
  # m参数停止的上界
  if(is.null(task_config$lower_m))  task_config$lower_m <- 3  # m参数的起始值
  # 开启模拟置信区间的真实值任务，而不是判决.
  if (is.null(task_config$sim_int)) task_config$sim_int <- FALSE 
  # 方差估计
  if(is.null(task_config$var.est.conf)) stop("The var est conf is not provided")
  if(!is.null(task_config$pre_vec)) { # 如果预先给定了性能向量，则不需要验证数据集等信息。
    return(task_config)
  }
  source("datasets/data_loader.R", encoding="UTF-8")
  source("mlAlgorithms/algor_loader.R", encoding="UTF-8")
  source("crossvalidations/cv_loader.R", encoding="UTF-8")
  # 数据集配置
  if (is.null(task_config$dataset.conf))    stop("Please specify data set configuration.")
  task_config$dataset.conf <- ValidateAndResolveDataSetConfiguration(task_config$dataset.conf)
  # 算法1配置
  if (is.null(task_config$algorithm1.conf)) stop("Please specify the first algorithm configuration.")
  task_config$algorithm1.conf <- ValidateAndResolveAlgorithmConfiguration(task_config$algorithm1.conf)
  # 算法2配置
  if (is.null(task_config$algorithm2.conf)) stop("Please specify the first algorithm configuration.")
  task_config$algorithm2.conf <- ValidateAndResolveAlgorithmConfiguration(task_config$algorithm2.conf)
  # 交叉验证配置
  if (is.null(task_config$crossvalidation.conf)) stop("Please specify the cross validation configuration.")
  # 14年f_m和C_m
  #if (!is.null(task_config$use14est)) task_config$use14est <- task_config$use14est
  # 加入f_m和C_m的类型
  if (is.null(task_config$est_type)) {
    task_config$est_type <- "typeI"
  } else {
    if(!task_config$est_type %in% names(mx2cv_seq_significant_test.est_types))
      stop("Unsupported est type.")
  }
  return(task_config)
}

mx2cv_seq_significant_test.perform_task <- function(task_config) {
  mu.vector <- task_config$pre_vec
  if(is.null(mu.vector)) {
      dataConf  <- task_config$dataset.conf
      algorConf <- task_config$algorithm1.conf
      algorConf2<- task_config$algorithm2.conf
      cvConf    <- task_config$crossvalidation.conf
      metricConf <- task_config$metric.conf
      DataInfo <- LoadDataSetGenerator(dataConf)
      DataGenerator <- DataInfo[[1]]
      DataPackages <- DataInfo[[2]]
      # 生成要检验的第一个算法
      AlgorInfo <- LoadAlgorithmGenerator(algorConf)
      AlgorGenerator <- AlgorInfo[[1]]
      AlgorPackages <- AlgorInfo[[2]]
      # 生成要检验的第二个算法
      AlgorInfo2 <- LoadAlgorithmGenerator(algorConf2)
      AlgorGenerator2 <- AlgorInfo2[[1]]
      AlgorPackages2 <- AlgorInfo2[[2]]
      metric.entry <- LoadPerformanceMetricGenerator(metricConf)[[1]]
      # 交叉验证类型必须为增量式mx2bcv.
      if(cvConf$name != 'mx2bcv_inc' && cvConf$name != 'mxkrcv_inc') {
        stop("cross validation needed: increaseBalancedMx2cv || mxkrcv_inc");
      }
      # 生成要使用的交叉验证
      CrossValidationInfo <- LoadCrossValidationGenerator(crossvalidation.name = cvConf$name)
      CrossValidationGenerator <- CrossValidationInfo[[1]]
      CrossValidationPackages <- CrossValidationInfo[[2]]
      
      WorkerInit(DataPackages)
      WorkerInit(AlgorPackages)
      WorkerInit(AlgorPackages2)
      WorkerInit(CrossValidationPackages)
      
      cvConf_org <- cvConf
      data <- DataGenerator(dataConf)
      n <- nrow(data)    
      cvConf$data <- data
  }
  
  
  alpha <- task_config$alpha  # 第一类错误概率
  beta  <- task_config$beta   # 第二类错误概率
  veConf <- task_config$var.est.conf  # 方差估计配置
  delta <- task_config$delta  # 原假设中的delta0
  upper_m <- task_config$upper_m  # m参数停止的上界
  fixed_m <- task_config$fixed_m
  relative <- task_config$relative # 相对误差
  lower_m <- 3  # m参数的起始值
  if(!is.null(task_config$lower_m)) {
    lower_m <- task_config$lower_m 
  }
  
  cur_m <- lower_m  # 存储当前的m值  
  test_result <- 0
  muv1 <- c()  # 存储第一个机器学习算法的性能
  muv2 <- c()  # 存储第二个机器学习算法的性能
  name.vec <- c("m", "delta_0", "delta_1", "var_est", "I_l", "I_r", "mu_algor1","mu_algor2","mu_diff", "prob_rej_H0", "prob_rej_H1")
  verbose.table <- c()
  pre_decision <- 0
  sim_int_result <- c()
  esti_funcs <- mx2cv_seq_significant_test.est_types[[task_config$est_type]]
  cm_func <- esti_funcs$cm
  fm_func <- esti_funcs$fm
  type1error <- 0
  type2error <- 0
  for(cur_m in lower_m:upper_m) {
    if(!is.null(fixed_m)) {
      cur_m <- fixed_m
    }
    # 计算c_m和f_m的估计
    c_m_est <- cm_func(cur_m, task_config) # 统计量所服从的t分布前的常数
    f_m_est <- fm_func(cur_m, task_config) # 统计量所服从的t分布的自由度
    # 计算机器学习算法的性能估计
    # 计算机器学习算法的性能估计
    muv1 <- NULL
    muv2 <- NULL
    mu_diff_vec <- NULL
    if(is.null(mu.vector)) {
        cvConf$m <- cur_m    
        cvConf <- CrossValidationGenerator(cvConf)
        partition.set <- cvConf$splits_new
        cvres1 <- TrainAndTestForCrossValidationEstimatorWithPartitionSet(data, partition.set, AlgorGenerator, algorConf, metric.entry=metric.entry, metric.conf=metricConf)
        cvres2 <- TrainAndTestForCrossValidationEstimatorWithPartitionSet(data, partition.set, AlgorGenerator2, algorConf2, metric.entry=metric.entry, metric.conf=metricConf)
        # 存储两个算法的性能估计值
        muv1 <- c(muv1, cvres1[[2]])
        muv2 <- c(muv2, cvres2[[2]])
        mu_diff_vec <- muv1 - muv2
    } else {
      #从mu.vector 中取出muv1, muv2和mu_diff_vec
      mu.length <- length(mu.vector)
      mu.unit.length <- mu.length /3
      muv1 <- mu.vector[1:(2*cur_m)]
      muv2 <- mu.vector[(mu.unit.length+1):(mu.unit.length+2*cur_m)]
      mu_diff_vec <- mu.vector[(2*mu.unit.length+1):(2*mu.unit.length+2*cur_m)]
    }
    if(relative == T) {
      mu_diff_vec <- mu_diff_vec / muv1
    }
    mu_diff <- mean(mu_diff_vec)
    # 计算机器学习算法性能估计的方差估计
    ve.estimator <- loadVarEstForOneExprInfo(veConf$name)
    veConf$m <- cur_m
    var_est <- ve.estimator(c(mu_diff_vec, mu_diff), veConf)
    # 计算右边界
    I_l = delta -  sqrt(var_est) * qt(p = 1-alpha, f_m_est)
    I_r = delta + c_m_est * sqrt(var_est) * qt(p = 1-alpha, f_m_est)
    if(task_config$sim_int) {
      sim_int_result <- rbind(sim_int_result, t(c(I_l, I_r)))
      next()
    }
    if(mu_diff > I_r) { # 接受H1
      pre_decision <- 1
      verbose.table <- rbind(verbose.table, c(cur_m, delta, var_est, I_r, mean(muv1), mean(muv2), mu_diff, pre_decision))
      test_result <- pre_decision
      break()
    }
    verbose.table <- rbind(verbose.table, c(cur_m, delta, var_est, I_r, mean(muv1), mean(muv2), mu_diff, pre_decision))
    if(!is.null(fixed_m) && cur_m == fixed_m) {
      test_result <- pre_decision
      break()
    }
  }
  if(task_config$sim_int) {
    row.names(sim_int_result) <- lower_m:upper_m
    return(sim_int_result)
  }
  test_result <- t(c(cur_m, test_result))
  colnames(test_result) <- c("m.stop", "test.result")
  result <- list( "verbose" = verbose.table, "test.result" = test_result, "mu.vec"= mu_diff_vec)
  return(result)
}


mx2cv_seq_significant_test.write_output <- function(result) {
  
}