#This file computes the divergence between xis and their means n/4
source('./all_kinds_cv.R')

#the distance formula is 1/C_m_2 \sum_{i=1}^C_m_2 |x_i - n/4|
abstractDistance<-function(n, m){
  #generate random m*2 cv groups
  rcvsplit<-randomMtimesKCV(m,n)
  combs<-combn(m,2)
  count<-ncol(combs)
  xis<-rep(NA,count)
  for(i in 1:count){
   split1<-rcvsplit[[combs[1,i]]]
   split2<-rcvsplit[[combs[2,i]]]
   xis[i]<-min(length(intersect(split1[[1]],split2[[1]])),length(intersect(split1[[1]],split2[[2]])))
  }
  #computing distance
  xis<-abs(xis-round(n/4))  
  distance<- mean(xis)
  return(distance)
}

#the distance formula is max_{i in [1, C_m^2]}|x_i - n/4|
#For this purpose, we 
maxDistance1<-function(n,m){
  rcvsplit<-randomMtimesKCV(m,n)
  combs<-combn(m,2)
  count<-ncol(combs)
  xis<-rep(NA,count)
  for(i in 1:count){
    split1<-rcvsplit[[combs[1,i]]]
    split2<-rcvsplit[[combs[2,i]]]
    xis[i]<-min(length(intersect(split1[[1]],split2[[1]])),length(intersect(split1[[1]],split2[[2]])))
  }
  xis<-abs(xis-round(n/4))
  max_x<-max(xis)  
  return(max_x)
}

maxDistance<-function(m,n,rcvsplit){
  combs<-combn(m,2)
  count<-ncol(combs)
  xis<-rep(NA,count)
  for(i in 1:count){
    split1<-rcvsplit[[combs[1,i]]]
    split2<-rcvsplit[[combs[2,i]]]
    xis[i]<-min(length(intersect(split1[[1]],split2[[1]])),length(intersect(split1[[1]],split2[[2]])))
  }
  xis<-abs(xis-round(n/4))
  max_x<-max(xis)  
  return(max_x)
}


implAbstractDistance<-function(){
  n<-512
  times<-10
  distances<-rep(NA,times)
  for(i in 1:times){
    m<-4*i-1
    rpt<-10
    sampledis<-rep(NA,rpt)
    for(j in 1:rpt){
      sampledis[j]<-abstractDistance(n,m)
    }
    distances[i]<-mean(sampledis)
  }
}

implMaxDistance<-function(){
  n<-512
  times<-20
  distances<-rep(NA,times)
  for(i in 1:times){
    m<-i+1
    rpt<-1000
    sampledis<-rep(NA,rpt)
    for(j in 1:rpt){
      sampledis[j]<-maxDistance(n,m)
    }
    distances[i]<-mean(sampledis)
  }
}

generateXisForMTwoCVs<-function(n,m)
{
  rcvsplit<-randomMtimesKCV(m,n)
  combs<-combn(m,2)
  count<-ncol(combs)
  xis<-rep(NA,count)
  for(i in 1:count){
    split1<-rcvsplit[[combs[1,i]]]
    split2<-rcvsplit[[combs[2,i]]]
    xis[i]<-length(intersect(split1[[1]],split2[[1]]))
  }
  return(xis)
}

implXi<-function(){
  m=7
  rep=10000
  ximat<-matrix(rep(NA, rep*length(combn(m,2))), rep)
  for(i in 1:rep)
    ximat[i,]<-generateXisForMTwoCVs(20,m)
}

f<-function(){
  v<-rep(NA, 1000)
  for(i in 1:1000){
    m=n=k=i
    p=0.1
    v[i]<-qhyper(p,m,n,k)
    v[i]<-v[i]-(k/2)
  }
  #m=n=k=1000
  #plot(phyper(0:(k+1), m,n,k))
  #plot(dhyper(0:(k+1),m,n,k))
}

fun<-function(s,k){
  a<-vector("list")
  if(k==1)
  {
    for(j in 0:s)
      a[[j+1]]<-j
    return(a)
  }
  for(i in 0:s){
      s1<-s-i
      b<-fun(s1,k-1)
      for(j in 1:length(b))
        a[[length(a)+1]]<-append(i, b[[j]])
  }
  return(a)
}




maxDistribution<-function(N,m){
 # N=40
#  m=7
  n1=n2=k=N/2
  #plot(dhyper(0:k,n1,n2,k))
  ds<-dhyper(0:k,n1,n2,k)
  #Fs<-phyper(0:k,n1,n2,k)
  if(N%%4!=0)
    stop("cannot execute")
  ave<-N/4
  ds_z<-rep(NA, ave+1)
  for(i in ave:k){
    if(i==ave){
      ds_z[1]<-ds[ave+1]
      next
    }
    ds_z[i-ave+1]<-(ds[2*ave-i+1]+ds[i+1])
  }
  
  #plot(ds_z)
  fs_z<-rep(NA,ave+1)
  for(i in 1:(ave+1))
    fs_z[i]<-sum(ds_z[1:i])
  cnt<-factorial(m)/factorial(m-2)/factorial(2)  
  probs_maxdis<-1-fs_z**cnt
  return(probs_maxdis)
}  

cntOfLevel<-function(n, m, lev){
  distri<-maxDistribution(n,m)
  return(max(which(distri>lev)-1))
}

anonymous<-function(lev,nr){
  #compute the maxDistribution values.  
  cntTable<-matrix(rep(NA, nr*10), nr)
  for(i in 1:nr){
    N=4*i
    print(i)
    for(m in 2:11){
      cntTable[i,m-1]<-cntOfLevel(N,m,lev)
    }
  }
  write.csv(cntTable, file="C:/table.txt")
}