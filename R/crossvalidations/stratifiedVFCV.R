stratifiedStandardVFCV.Generator<-function(n, n1, v, J, data) {
  groups <- vector("list", v)
  y <- data[[2]]
  fctr <- as.double(c(levels(factor(y))))
  fctr_count <- length(fctr)
  indexArrays <- vector("list", fctr_count)
  orderedArray <- c()
  for(fctr_idx in 1:fctr_count) {
    orderedArray <- c(orderedArray, which(y ==  fctr[fctr_idx:fctr_idx]))
  }  
  rep_count <- ceiling(length(y)/v)
  for(v_idx in 1:v) {
    bool_v <- rep(FALSE, v)
    bool_v[v_idx] <- TRUE
    bool_vec <- rep(bool_v, rep_count)
    bool_vec <- bool_vec[1:length(y)]
    groups[[v_idx]] <- orderedArray[which(bool_vec)]
  }
  return(groups)
}

stratifiedStandardVFCV.Prepackages <- c()

