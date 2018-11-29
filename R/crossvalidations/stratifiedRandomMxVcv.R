stratifiedRandomMxVcv.Generator<-function(cvConf){
  J <- cvConf$m
  n <- cvConf$n
  v <- cvConf$v
  data <- cvConf$data
  mgroupsCVs<-vector("list",J)
  for(r in 1:J){
    mgroupsCVs[[2*r-1]]<-stratifiedRandomMxVcv.stratifiedStandardVFCV(n, n1, v, data)
    mgroupsCVs[[2*r]] <- rev(mgroupsCVs[[2*r-1]])
  }
  return(mgroupsCVs)
}

stratifiedRandomMxVcv.Prepackages <- c()



stratifiedRandomMxVcv.validation <- function(cvConf) {
  if( is.null(cvConf$m) ) return(FALSE)
  if( is.null(cvConf$n) ) return(FALSE)
  if( is.null(cvConf$v) ) return(FALSE)
  return(TRUE)
}

stratifiedRandomMxVcv.stratifiedStandardVFCV<-function(n, n1, v, data) {
  stop("no randomness in this algorithm.")
  
  groups <- vector("list", v)
  y <- data[, ncol(data)]
  fctr <- c(levels(factor(y)))
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


