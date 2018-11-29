stratifiedBalancedMx2cv.Generator <- function(cvConf){
  n <- cvConf$n
  J <- cvConf$m
  data <- cvConf$data
  if (J >= 143 )  stop("J > 143.")  
  if ( n< J + 1 ) stop("J > n-1")
  orthArray<-NULL  
  orthArray3 <- data.frame(f1=c(1,1,-1,-1),f2=c(1,-1,1,-1),f3=c(1,-1,-1,1))
  orthArray7 <- data.frame(f1=c(-1,-1,-1,-1,1,1,1,1),f2=c(-1,-1,1,1,-1,-1,1,1),
                           f3=c(-1,1,-1,1,-1,1,-1,1),f4=c(1,1,-1,-1,-1,-1,1,1),
                           f5=c(1,-1,1,-1,-1,1,-1,1),f6=c(1,-1,-1,1,1,-1,-1,1),
                           f7=c(-1,1,1,-1,1,-1,-1,1))
  orthArray11 <- data.frame(f1=c(1,-1,1,-1,-1,-1,1,1,1,-1,1,-1),
                            f2=c(1,1,-1,1,-1,-1,-1,1,1,1,-1,-1),
                            f3=c(-1,1,1,-1,1,-1,-1,-1,1,1,1,-1),
                            f4=c(1,-1,1,1,-1,1,-1,-1,-1,1,1,-1), 
                            f5=c(1,1,-1,1,1,-1,1,-1,-1,-1,1,-1),
                            f6=c(1,1,1,-1,1,1,-1,1,-1,-1,-1,-1),
                            f7=c(-1,1,1,1,-1,1,1,-1,1,-1,-1,-1),
                            f8=c(-1,-1,1,1,1,-1,1,1,-1,1,-1,-1),
                            f9=c(-1,-1,-1,1,1,1,-1,1,1,-1,1,-1),
                            f10=c(1,-1,-1,-1,1,1,1,-1,1,1,-1,-1),
                            f11=c(-1,1,-1,-1,-1,1,1,1,-1,1,1,-1))
  if ( J <= 3 ) {
    orthArray <- orthArray3[,1:J]
  } else if ( J <= 7 ) {
    orthArray <- orthArray7[, 1:J]
  } else if ( J <= 11 ) {
    orthArray <- orthArray11[, 1:J]
  }else{    
    plan <- oa.design(nruns = J + 1, nfactors = J, nlevels=2)
    oa<-matrix(t(as.numeric(as.matrix(plan))),J + 1)
    orthArray<-data.frame((oa-1.5)*2)
  }    
  n1 <- round(n/2)
  blocks<-stratifiedBalancedMx2cv.stratifiedStandardVFCV(n, n1, nrow(orthArray[1]), data)
  mgroupsCVs<-vector("list", J)  
  for(i in 1:J){
    split<-orthArray[i]       
    cvs<-vector("list",2)
    for(b in 1:nrow(split)){      
      if(split[b,1]==1){        
        if(is.null(cvs[[1]])){
          cvs[[1]]<-blocks[[b]]
        }else{
          cvs[[1]]<-cbind(t(as.numeric(cvs[[1]])),t(as.numeric(blocks[[b]])))
        }
      }else{
        if(is.null(cvs[[2]])){
          cvs[[2]]<-blocks[[b]]
        }else{
          cvs[[2]]<-cbind(t(as.numeric(cvs[[2]])),t(as.numeric(blocks[[b]])))
        }
      }
    }
    cvs[[1]]<-sort(cvs[[1]])
    cvs[[2]]<-sort(cvs[[2]])
    mgroupsCVs[[2*i-1]]<-cvs
    mgroupsCVs[[2*i]] <- rev(mgroupsCVs[[2*i-1]])
  }  
  return(mgroupsCVs)
}

stratifiedBalancedMx2cv.Prepackages <- c("DoE.base")

stratifiedBalancedMx2cv.validation <- function(cvConf) {
    if(is.null(cvConf$n) ) return(FALSE)
    if( is.null(cvConf$m) ) return(FALSE)
    return(TRUE)
}


stratifiedBalancedMx2cv.stratifiedStandardVFCV<-function(n, n1, v, data) {
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
