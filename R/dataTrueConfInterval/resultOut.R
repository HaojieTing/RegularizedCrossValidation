print(paste('Sample size:',n))
print(paste('Size of train examples:',ntrain))
print(paste("Sample dimension:",d))
print(paste("Repitition count:", rpt))
print(paste("Split count", sptrpt))
print(paste("alpha for quntaile:",alpha))
print(paste('Data type:' , datatype))
print(paste("Machine learning algor. type:", mltype))
print(paste("the 1st Machine learning algorithm:",algorithm1))
print(paste("the 2nd Machine learning algorithm:",algorithm2))
#---------------------------------------------------------------------
if(!exist){
  sname = digest(list("trueci",algorithm1,alpha,n,ntrain,d,rpt,sptrpt,datatype,algorithm2,mltype),algo="md5")
  filename  <- paste("../trueCIData/",sname,".RData",sep="")
  count <- 1
  while(file.exists(filename)){
    filename <- paste("../trueCIData/",count,".",sname,".RData",sep="")
    count <- count + 1  }
  save(muv_1, muv_2,ci_algor1, ci_algor2, ci_algor1_algor2, ci_algor2_algor1, algorithm1,alpha,n,ntrain,d,rpt,sptrpt,datatype,algorithm2,mltype, file=filename)  
}