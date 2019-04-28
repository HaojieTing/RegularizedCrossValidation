# 计算所有数据集上算法的均值和相关性。
source("./varianceEstimator/var_est3_mx2cv.R", encoding="UTF-8")
source("./varianceEstimator/var_est_brhs4.R", encoding="UTF-8")

computeStatisticForEstMatrix <- function(est.matrix) {
  cor.matrix.A <- cor(est.matrix)
  #计算rho_1
  n.col.per.unit <- ncol(est.matrix)
  rho_1_vec <- c()
  for(j in 1:(n.col.per.unit/2)) {
    rho_1_vec <- c(rho_1_vec, cor.matrix.A[2*j-1, 2*j])
    rho_1_vec <- c(rho_1_vec, cor.matrix.A[2*j, 2*j-1])
  }
  rho.1 <- mean(rho_1_vec)
  #计算rho_2
  for(j in 1:(n.col.per.unit/2)) {
    cor.matrix.A[2*j-1, 2*j] <- NA
    cor.matrix.A[2*j, 2*j-1] <- NA
  }
  diag(cor.matrix.A) <- NA
  rho.2 <- mean(cor.matrix.A, na.rm = T)
  return(c(rho.1, rho.2))
}

# 计算组块3x2交叉验证的相关统计量
info.mu.matrix.b3x2 <- function(mu.matrix) {
  mu.A           <- mean(rowMeans(mu.matrix))
  var.A          <- var(rowMeans(mu.matrix))
  var_est_hat_vec_A <- c()
  for(i in 1:nrow(mu.matrix)) { var_est_hat_vec_A <- c(var_est_hat_vec_A, var_est3_mx2cv.EstForOneExpr(c(mu.matrix[i,], mean(mu.matrix[i,])), list(m=3)))}
  var_est_hat_A <- mean(var_est_hat_vec_A)
  rho.vec.A <- computeStatisticForEstMatrix(mu.matrix)
  rho1.A  <- rho.vec.A[1]
  rho2.A  <- rho.vec.A[2]
  stat.A    <- c(mu.A,    var.A,    var_est_hat_A,    rho1.A,    rho2.A)
  return(stat.A)
}

# 计算组块BRHS的相关统计量
info.mu.matrix.brhs4 <- function(mu.matrix) {
  mu.A           <- mean(rowMeans(mu.matrix))
  var.A          <- var(rowMeans(mu.matrix))
  var_est_hat_vec_A <- c()
  for(i in 1:nrow(mu.matrix)) { var_est_hat_vec_A <- c(var_est_hat_vec_A, var_est_brhs4.EstForOneExpr(c(mu.matrix[i,], mean(mu.matrix[i,])), list()))}
  var_est_hat_A <- mean(var_est_hat_vec_A)
  # 计算相关系数
  cor.matrix <- cor(mu.matrix)
  diag(cor.matrix) <- NA
  rho <- mean(cor.matrix, na.rm = T)
  stat.A <- c(mu.A, var.A, var_est_hat_A, rho)
  return(stat.A)
}


info.dataset.b3x2 <- function(dataset.filename, contain.diff = T) {
  data.set       <- read.table(dataset.filename)
  ncol.data.set <- ncol(data.set)
  if(ncol.data.set %% 6 != 0) stop("invalid matrix format.")
  count.mu <- ncol.data.set / 6
  stat.matrix <- c()
  for(i in 1:count.mu) {
    mu_matrix_A    <- as.matrix(data.set[,((i-1)*6+1):(i*6)])
    stat.A <- info.mu.matrix.b3x2(mu_matrix_A)
    stat.matrix <- rbind(stat.matrix, stat.A)
  }
  if(contain.diff == F) {
    combs <- combn(count.mu, 2)
    combs.count <- ncol(combs)
    for(i in 1:combs.count) {
      comb.a <- c(combs[,i])
      index.one <- comb.a[1]
      mu_matrix_A    <- as.matrix(data.set[,((index.one-1)*6+1):(index.one*6)])
      index.two <- comb.a[2]
      mu_matrix_B    <- as.matrix(data.set[,((index.two-1)*6+1):(index.two*6)])
      mu_matrix_diff <- mu_matrix_B - mu_matrix_A
      stat.diff <- info.mu.matrix.b3x2(mu_matrix_diff)
      stat.matrix <- rbind(stat.matrix, stat.diff)
    }
  }
  colnames(stat.matrix) <- c("mu", "var", "var.est", "rho1", "rho2")
  return(stat.matrix)
}


info.dataset.brhs4 <- function(dataset.filename, contain.diff = T) {
  data.set <- read.table(dataset.filename)
  ncol.data.set <- ncol(data.set)
  if(ncol.data.set %% 4 != 0) stop("invalid matrix format.")
  count.mu <- ncol.data.set / 4
  stat.matrix <- c()
  for(i in 1:count.mu) {
    mu_matrix_A    <- as.matrix(data.set[,((i-1)*4+1):(i*4)])
    stat.A <- info.mu.matrix.brhs4(mu_matrix_A)
    stat.matrix <- rbind(stat.matrix, stat.A)
  }
  if(contain.diff == F) {
    combs <- combn(count.mu, 2)
    combs.count <- ncol(combs)
    for(i in 1:combs.count) {
      comb.a <- c(combs[,i])
      index.one <- comb.a[1]
      mu_matrix_A    <- as.matrix(data.set[,((index.one-1)*4+1):(index.one*4)])
      index.two <- comb.a[2]
      mu_matrix_B    <- as.matrix(data.set[,((index.two-1)*4+1):(index.two*4)])
      mu_matrix_diff <- mu_matrix_B - mu_matrix_A
      stat.diff <- info.mu.matrix.brhs4(mu_matrix_diff)
      stat.matrix <- rbind(stat.matrix, stat.diff)
    }
  }
  colnames(stat.matrix) <- c("mu", "var", "var.est", "rho")
  return(stat.matrix)
}

# 模拟数据Bengio2003Sim上的均值、方差、方差估计和相关性。

dataset.sim2.b3x2 <- "D:/datasets/Zzzz/two_gaussian_sim2_3x2bcv_20190425111158"
dataset.sim2.brhs <- "D:/datasets/Zzzz/two_gaussian_sim2_brhs4_20190425111212"

info.dataset.b3x2(dataset.sim2.b3x2)
info.dataset.brhs4(dataset.sim2.brhs)

dataset.sim4.b3x2 <- "D:/datasets/Zzzz/two_gaussian_sim4_3x2bcv_20190425111659"
dataset.sim4.brhs <- "D:/datasets/Zzzz/two_gaussian_sim4_brhs4_20190425120757"

info.dataset.b3x2(dataset.sim4.b3x2)
info.dataset.brhs4(dataset.sim4.brhs)

# Letter数据上的均值、方差、方差估计和相关性。

dataset.letter.b3x2 <- "D:/datasets/Zzzz/letter_3x2bcv_20190425112536"
dataset.letter.brhs <- "D:/datasets/Zzzz/letter_brhs4_20190425113403"

info.dataset.b3x2(dataset.letter.b3x2)
info.dataset.brhs4(dataset.letter.brhs)

# 5组uci数据上的均值、方差、方差估计和相关性。

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
            print(info.dataset.b3x2(file.abs.name))
          } else {
            print(info.dataset.brhs4(file.abs.name))
          }
        }
      } 
    }
  }
}




# 10组uci数据上的均值、方差、方差估计和相关性。

uci_ten_datasets <- c("balance", "diabetes", "glass", "heart", "ionosphere", "iris", "vehicle", "wine", "yeast", "seed")
for(data.tag in uci_ten_datasets) {
  for(test_method in c("3x2bcv", "brhs4")) {
    str.prefix <- paste("rep_uci", data.tag, test_method, sep="_")
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
        print(info.dataset.b3x2(file.abs.name))
      } else {
        print(info.dataset.brhs4(file.abs.name))
      }
    }
  }
}