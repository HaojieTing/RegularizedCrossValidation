#########################################
#find common elements count of m groups 2 cv
########################################
perm <- function(n,k){choose(n,k) * factorial(k)}

statCommonElementCount<-function(mgroups){
    counts<-rep(NA, perm(length(mgroups),2)/2)  
    index <-1
    for(i in 1:(length(mgroups)-1)){
      group1<-mgroups[[i]]
      for(j in (i+1):length(mgroups)){
        group2<-mgroups[[j]]
        counts[index] <- length(intersect(group1[[1]],group2[[1]]))
        index<-index+1
      }
    }
    return(counts)
}


