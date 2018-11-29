set.seed(123)
rm(list=ls())
cv.types <- c("rhs", "rmx2cv", "bmx2cv", "brhs")
data.types <- c("krvskp", "optdigits", "page_block", "spambase", "wave")
data.sizes <- list("krvskp" = 200, "optdigits" = 300, 
                   "page_block" = 300, "spambase" = 250, "wave" = 300)
algor.types <- c("knn", "svm")
for(data.name in data.types) {
  for(algor.name in algor.types){
    cat(data.name, algor.name)
    for(cv.name in cv.types) {
     for(m in c(3,5,7)){
       rm("result")
       tag <- ""
       if(cv.name == "rhs" || cv.name == "brhs")
         tag <- paste(data.sizes[[data.name]]/2, m*2, sep="_")
       else {
         tag <- m
       }
       prefix <- "var_sim_uci"
       if(cv.name == "rmx2cv"||cv.name == "bmx2cv") {
         prefix <- "var_sim__uci"
       }
       file.name <- paste(cv.name, prefix, data.name, data.sizes[[data.name]],tag, algor.name, 1000, 100, sep="_")
       file.abs.name <- file.path("C:/Users/YJ/Desktop/stderr", file.name)
       load(file.abs.name)
       result <- result[[1]]
       mu.vec <- result[, ncol(result)]
       sample <- c()
       for(i in 1:20) sample <- c(sample, var(mu.vec[sample(x = 1:length(mu.vec), size = 50000, replace = T)]))
       se <- sd(sample)/sqrt(length(sample))
       #cat(mean(sample), '\t',sd(sample), "\t")
       cat(sample, "\t")
     }
    }
    cat("\n")
  }
  if(data.name == "optdigits") stop()
}