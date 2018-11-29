#
#
#
#

mx2_t_test_power.getDonominatorAndDOF <- function(task_config) {
  type <- task_config$type
  if (type == "mx2cv_seq_significant_test") {
    return(mx2_t_test_power.mx2bcvseqt.getDonominatorAndDOF(task_config))
  } else if(type == "diet_mx2cv_seq_significant_test") {
    return(mx2_t_test_power.dietterich.getDonominatorAndDOF(task_config))
  } else if(type == "combined_mx2cv_seq_t_test") {
    return(mx2_t_test_power.combined.getDonominatorAndDOF(task_config))  
  }
  return(NULL)
}

mx2_t_test_power.mx2bcvseqt.getDonominatorAndDOF <- function(task_config) {
  # 给定m, rho1 和rho2，计算自由度和分母的值。
  m <- task_config$m * 1.0
  rho1 <- task_config$rho1* 1.0
  rho2 <- task_config$rho2* 1.0
  sgm <- task_config$sigma* 1.0
  # 计算自由度
  dof <- (2*m*(1-rho2)-(1+rho1-2*rho2))*(2*m*(1-rho2)-(1+rho1-2*rho2))
  dof <- dof /(m*(1-rho1)*(1-rho1)+(m-1)*(1+rho1-2*rho2)*(1+rho1-2*rho2))
  # 计算分母
  cm <- (1+rho1+2*(m-1)*rho2)/(2*m-(1+rho1+2*(m-1)*rho2))
  cm <- sqrt(cm)
  sigmam <- sqrt(1/(2*m)*(1+rho1)+(m-1)/m*(1-1/(m-1)*rho1-rho2))*sgm
  don <- cm * sigmam
  return(list(dof= dof, don=don))
}

mx2_t_test_power.dietterich.getDonominatorAndDOF <- function(task_config) {
  m <- task_config$m * 1.0
  rho1 <- task_config$rho1* 1.0
  rho2 <- task_config$rho2* 1.0
  sgm <- task_config$sigma* 1.0
  # 自由度
  dof <- m
  # 分母
  don <- sgm * sqrt(1+rho1)
  return(list(dof= dof, don=don))
}

mx2_t_test_power.combined.getDonominatorAndDOF <- function(task_config) {
  m <- task_config$m * 1.0
  rho1 <- task_config$rho1* 1.0
  rho2 <- task_config$rho2* 1.0
  sgm <- task_config$sigma* 1.0
  # 自由度
  dof <- m
  # 分母
  don <- sgm * sqrt((1+rho1)/(2*m))
  return(list(dof= dof, don=don))
}


mx2_t_test_power.task_config_validation <- function(task_config) {
  if(is.null(task_config$delta0)) stop("Please provide delta0")
  if(is.null(task_config$delta1)) stop("Please provide delta1")
  if(is.null(task_config$alpha)) stop("Please provide alpha")
  if(is.null(task_config$beta)) stop("Please provide beta")
  if(is.null(task_config$rho1)) stop("Please provide rho1")
  if(is.null(task_config$rho2)) stop("Please provide rho2")
  if(is.null(task_config$sigma)) stop("Please provide sigma")
  if(is.null(task_config$m)) stop("Please provide m")
  if(is.null(task_config$type)) stop("Please provide type")
  if(is.null(task_config$step)) stop("Please provide step")
  if(is.null(task_config$low_bound)) stop("Please provide low_bound")
  if(is.null(task_config$up_bound)) stop("Please provide up_bound")
  if(is.null(task_config$plot)) task_config$plot <- TRUE
  if(is.null(task_config$plot_reflines)) task_config$plot_reflines <- TRUE
  if(is.null(task_config$loss_costs)) stop("Please provide loss costs")
  return(task_config)
}


mx2_t_test_power.perform_task <- function(task_config) {
  delta0 <- task_config$delta0
  delta1 <- task_config$delta1
  gap <- task_config$step
  bound.low <- task_config$low_bound
  bound.up <- task_config$up_bound
  alpha <- task_config$alpha
  beta <- task_config$beta
  test_config <- mx2_t_test_power.getDonominatorAndDOF(task_config)
  dfp <- test_config$dof
  don <- test_config$don
  a <- seq(bound.low, delta0-gap, gap)
  b <- seq(delta0, delta1, gap)
  c <- seq(delta1+gap, bound.up, gap)
  acc1 <- pt((delta0-a)/don-qt(p = 1-alpha,df = dfp), df = dfp)
  acc2 <- pt((delta1-b)/don+qt(p = 1-beta,df = dfp), df = dfp)-pt((delta0-b)/don-qt(p=1-alpha, df=dfp), df=dfp)
  acc3 <- 1-pt((delta1-c)/don+qt(p=1-beta, df=dfp), df=dfp)
  x <- c(a,b,c)
  y <- c(acc1, acc2, acc3)
  gu <- pt((delta0-x)/don-qt(p = 1-alpha, df = dfp), df = dfp)+1-pt((delta1-x)/don+qt(p = 1-beta,df = dfp), df = dfp)
  bc <- c(b, c)
  acc1_comp <- pt((delta0-bc)/don-qt(p = 1-alpha,df = dfp), df = dfp)
  ab <- c(a, b)
  acc3_comp <- 1-pt((delta1-ab)/don+qt(p=1-beta, df=dfp), df=dfp)
  # 画出精度线
  if(task_config$plot) {
    plot(y, x=x, cex=0.1, ylim=c(0,1))
     lines(gu, x=x)
    abline(v=delta0,lwd=1,col="red", lty=3)
    abline(v=delta1,lwd=1,col="red", lty=3)
    if(task_config$plot_reflines) {
      sep1 <- rep(0.1, length(x))
      sep2 <- rep(0.05, length(x))
      sep3 <- rep(0, length(x))
      sep4 <- rep(1, length(x))
      lines(sep1, x=x, lty=3)
      lines(sep2, x=x, lty=3)
      lines(sep3, x=x, lty=5)
      lines(sep4, x=x, lty=5)
      lines(1-sep2, x=x, lty=3)
      lines(1-sep1, x=x, lty=3)
      lines(acc1_comp, x= bc, lty=6, col='blue')
      lines(acc3_comp, x= ab, lty=6, col='blue')
    }
  }
  # 计算critical error
  critical.error.left <- gu[1:length(a)] - acc1
  critical.error.right<- gu[(length(a)+length(b)+1):length(x)]-acc3
  conser.error.left <- 1-gu[1:length(a)]
  conser.error.right <- 1- gu[(length(a)+length(b)+1):length(x)]
  acc.left  <- acc1
  acc.right <- acc3
  acc.middle <- acc2
  liberal.error <- gu[(length(a)+1):(length(a)+length(b))]
  # 取出cost值。
  costs <- task_config$loss_costs
  liberal.cost <- costs$liber
  cons.cost <- costs$cons
  crit.cost <- costs$crit
  acc.cost <- costs$acc
  # 计算loss值
  loss.left <- acc.left * acc.cost + critical.error.left * crit.cost + conser.error.left * cons.cost
  loss.right <- acc.right*acc.cost + critical.error.right *crit.cost + conser.error.right *cons.cost
  loss.middle <- acc.middle*acc.cost + liberal.error * liberal.cost
  losses <- c(loss.left, loss.middle, loss.right)
  return(list(
    critical.error.left = critical.error.left,
    critical.error.right = critical.error.right,
    conser.error.left = conser.error.left,
    conser.error.right = conser.error.right,
    liberal.error = liberal.error,
    loss = losses,
    mus = x
  ))
}

mx2_t_test_power.write_output <- function(task_config) {
  
}
