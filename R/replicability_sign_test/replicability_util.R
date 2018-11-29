const_for_replic <- function(test_count) {
  if(test_count<2)
    stop("Test number should be larger than 2")
  num<-rep(NA,test_count+1)
  for (i in 0:test_count)
  {
    if(i<2)
      num[i+1]<-choose(test_count-i,2)
    else if((test_count-i)<2)  
      num[i+1]<-choose(i,2)
    else num[i+1]<-choose(test_count-i,2)+choose(i,2)
  }
  return(num)
}


Replicability.compute <- function(typeI_counts, test_count) {
  k <- 0
  consistent_num <- 0
  alm_consist_num <- 0
  typeI_counts <- as.vector(typeI_counts)
  r <- const_for_replic(test_count)
  for (i in 1:length(typeI_counts))
  {
    type1_count <- typeI_counts[i]
    if(type1_count == test_count || type1_count == 0)
      consistent_num <- consistent_num+1
    if(type1_count >= (test_count - 1)|| (type1_count <= 1))
      alm_consist_num <- alm_consist_num+1    
    k <- k + r[type1_count + 1]
    
  }
  k<-k/choose(test_count,2)/length(typeI_counts)
  final<-list(consistent_num, alm_consist_num, k)
  names(final) <- c("consistent count" , "almost consistent count", "Replicability")
  return(final)
}
