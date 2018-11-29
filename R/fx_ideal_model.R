setwd('D:/balancedMTimesTwoCV')

source('./all_kinds_cv.R')
source('./datas.R')
source('./util.R')
source('./regressioner.R')

N <- 10  #number of observants
V <-2
rpt <- 50000#data repetition

samples <- vector("list",rpt)
#for(i in 1:rpt)
#  samples[[i]]<-rnorm(N)
for(i in 1:rpt){
  covm<-matrix(rep(1.5,N**2),N)
  for(k in 1:N)
    covm[k,k]<-1
  samples[[i]]<-mvrnorm(1, rep(0,N), diag(N))
}

groupList<-vector("list",rpt)
for(i in 1:rpt){
  groups <- randomVFCV(N,V)
  groupList[[i]] <- groups
}

excnts=seq(0, N/2, 5)

loss_excnts = vector("list",length(excnts))
for(i in 1:length(excnts)){
  cnt =excnts[i]  
  loss_rpt = vector("list",length(rpt))  
  for(r in 1:rpt)
  {
    groups<-groupList[[r]]
    regroups<-randomExchangeingObservants(groups,cnt)    
    data<-samples[[r]]    
    loss_V=vector("list",V)
    for(v in 1:V){
      pre<-mean(data[-regroups[[v]]])            
      loss = (pre-data[regroups[[v]]])**2
      loss_V[[v]] <- loss
    }    
    ls <-append(loss_V[[1]],loss_V[[2]])
    loss_rpt[[r]] <- ls
  }
  loss_excnts[[i]]<-loss_rpt
}

mu <- matrix(rep(NA, length(loss_excnts)*rpt*2), ncol= rpt)
for(i in 1:length(loss_excnts)){
  m<-loss_excnts[[i]]
  cnt <- excnts[i]
  group <- rep(NA, length(m[[1]])/2)
  for(k in 1:length(group)){
    group[k] <-k  
  }   
  for(j in 1:rpt)
  {
    mu[i,j] <- sum(m[[j]][group])/length(m[[j]][group])  
    mu[length(loss_excnts)+i,j] <- sum(m[[j]][-group])/length(m[[j]][-group])
  }
}

coval <- matrix(rep(NA,4*length(loss_excnts)),nrow=4)
for(i in 1:ncol(coval))
  coval[1,i] <- cov(mu[i,],mu[1,])
for(i in 1:ncol(coval))
  coval[2,i] <-cov(mu[length(loss_excnts)+i,],mu[length(loss_excnts)+1,])
for(i in 1:ncol(coval))
  coval[3,i] <-cov(mu[i,],mu[length(loss_excnts)+1,])
for(i in 1:ncol(coval))
  coval[4,i] <-cov(mu[length(loss_excnts)+i,],mu[1,])
plot(N/2-excnts,coval[1,], xlab ="x", ylab="f(x)",type="o",col="red")
lines(N/2-excnts,coval[2,],col="blue",type="o",pch=22)
lines(excnts,coval[3,],col="black",type="o")
lines(excnts,coval[4,],col="green",type="o")


