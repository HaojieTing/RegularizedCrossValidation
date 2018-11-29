approx_fx<-function(x, n, parms){
  ret<-parms$sigma * x + parms$ omega * x*(x-1)+parms$gamma*(n/2-x)**2+parms$tau1 *x * (n/2-x)+parms$tau2 *x * (n/2-x)
  ret<-ret*4/n**2
  return(ret)
}



plot_fx<-function(filename, n , ps, parmv=NULL){
  
  data<-read.csv(file=filename,header=FALSE)  
  
  lty="p"
  for( i in 1:(nrow(data)/5)){
    p=ps[i]
    approx_fxs<-NULL
    if(!is.null(parmv)){      
      approx_fxs<-sapply(0:(n/2), approx_fx, n,parmv[[i]])
    }
    
    ylims<-c(min(data[((i-1)*5+1):((i-1)*5+5),]),max(data[((i-1)*5+1):((i-1)*5+5),]))
    if(!is.null( approx_fxs)){
     ylims<-c(min(c(ylims, approx_fxs)),max(c(ylims, approx_fxs)))
    }
    line(0:(n/2),data[(i-1)*5+1,], xlab ="x", ylab="f(x)",ylim=ylims,type=lty,col="red",main=paste(paste(paste("n=",n,sep="")),paste(paste(",p=",p,sep=""))),pch=21)
    #lines(0:(n/2),data[(i-1)*5+2,],col="blue",type=lty,pch=22)
    #lines(0:(n/2),data[(i-1)*5+3,],col="black",type=lty,pch=23)
    #lines(0:(n/2),data[(i-1)*5+4,],col="green",type=lty,pch=24)
    #lines(0:(n/2),data[(i-1)*5+5,],col="purple",type=lty,pch=25)
    if(!is.null( approx_fxs)){
      parms<-parmv[[i]]
      lines(0:(n/2), approx_fxs,type="l",lty=2,col="darkgreen")
    }
  }
}


plot_hyper<-function(ns){
  par(mfrow=c(length(ns)/2+1,2),xpd=TRUE)
  for(n in ns){
    a<-dhyper(0:(n/2), (n/2),(n/2),(n/2))
    plot(0:(n/2),a,type="h",xlab="x",ylab="prob.", main=paste("n=",n,sep=""),col="blue")
  }
}

hyperProb<-function(k,n) {
  return(dhyper(k, n/2, n/2, n/2))
}


probstratifiedhyper<-function(k, n1, n2){
  if(n1%%2!=0||n2%%2!=0){
    stop("")  
  }    
  prob<-0
  for(a in 0:min(k,n1/2))
  {
    if(k-a>n2/2)
      next
    prob<-prob+hyperProb(a,n1)*hyperProb(k-a, n2)
  }
  return(prob)
}

n=320
types<-seq(0, 40,2)
mat<-matrix(rep(NA,(1+n/2)*length(types)),ncol=1+n/2)
idx <- 1
for(ot in types){
  mat[idx,]<-sapply(0:(n/2),probstratifiedhyper,ot,n-ot)-sapply(0:(n/2),hyperProb, n)
  idx <- idx + 1
}
boxplot(mat)
#plot hyper geometric prob. density  for several sample sizes.
ns=c(40,80)
plot_hyper(c(40,80))
#plot f(x) figure and approximation curve with parmeters.
par(mfrow=c(12,4),xpd=TRUE)
##read parameters
parmmat<-read.csv(file="C:/Users/WANG/Desktop/paper/BMTWOCV/results/parms.txt")
##low dimensional regression
plot_fx("C:/Users/WANG/Desktop/paper/BMTWOCV/results/fxregr40nooutlier.txt", 40, c(3,10), parmv=list( parmmat[which(parmmat$type=='ldnooutlierRegr_40_3'),],parmmat[which(parmmat$type=='ldnooutlierRegr_40_10'),]))
plot_fx("C:/Users/WANG/Desktop/paper/BMTWOCV/results/fxregr80nooutlier.txt", 80, c(3,10), parmv=list( parmmat[which(parmmat$type=='ldnooutlierRegr_80_3'),],parmmat[which(parmmat$type=='ldnooutlierRegr_80_10'),]))
plot_fx("C:/Users/WANG/Desktop/paper/BMTWOCV/results/fxregr40outlier.txt", 40, c(3,10), parmv=list( parmmat[which(parmmat$type=='ldoutlierRegr_40_3'),],parmmat[which(parmmat$type=='ldoutlierRegr_40_10'),]))
plot_fx("C:/Users/WANG/Desktop/paper/BMTWOCV/results/fxregr80outlier.txt", 80, c(3,10), parmv=list( parmmat[which(parmmat$type=='ldoutlierRegr_80_3'),],parmmat[which(parmmat$type=='ldoutlierRegr_80_10'),]))

##high dimension regression 
plot_fx("C:/Users/WANG/Desktop/paper/BMTWOCV/results/fxhdregr40.txt", 40, c(100,200), parmv=list( parmmat[which(parmmat$type=='hdRegr_40_100'),],parmmat[which(parmmat$type=='hdRegr_40_200'),]))
plot_fx("C:/Users/WANG/Desktop/paper/BMTWOCV/results/fxhdregr80.txt", 80, c(100,200), parmv=list( parmmat[which(parmmat$type=='hdRegr_80_100'),],parmmat[which(parmmat$type=='hdRegr_80_200'),]))

##low and high dimension classification
plot_fx("C:/Users/WANG/Desktop/paper/BMTWOCV/results/fxclas48cart.txt", 48, c(10,20,100,200), parmv=list(parmmat[which(parmmat$type=='cartClas_48_10'),],parmmat[which(parmmat$type=='cartClas_48_20'),],parmmat[which(parmmat$type=='cartClas_48_100'),],parmmat[which(parmmat$type=='cartClas_48_200'),]))
plot_fx("C:/Users/WANG/Desktop/paper/BMTWOCV/results/fxclas96cart.txt", 96, c(10,20,100,200), parmv=list(parmmat[which(parmmat$type=='cartClas_96_10'),],parmmat[which(parmmat$type=='cartClas_96_20'),],parmmat[which(parmmat$type=='cartClas_96_100'),],parmmat[which(parmmat$type=='cartClas_96_200'),]))


#the following code is plotting the boxplots for the comparision of B M*2CV and R M*2 CV
dir='C:/Users/WANG/Desktop/paper/BMTWOCV/results/ComparisonDataPlot'
fn= '/ldnooutRgr_40_3.txt'
#fn= '/ldnooutRgr_40_10.txt'
#fn= '/ldnooutRgr_80_3.txt'
#fn= '/ldnooutRgr_80_10.txt'
#fn= '/ldoutRgr_40_3.txt'
#fn= '/ldoutRgr_40_10.txt'
#fn= '/ldoutRgr_80_3.txt'
#fn= '/ldoutRgr_80_10.txt'
#fn= '/hdRgr_40_100.txt'
#fn= '/hdRgr_40_200.txt'
#fn= '/hdRgr_80_100.txt'
#fn= '/hdRgr_80_200.txt'
f=paste(dir, fn,sep="")
data<-read.csv(file=f, header=FALSE)
data<-t(data)
boxplot(data,names=c("sigma","omega","gamma","var","sigma","omega","gamma","var","sigma","omega","gamma","var"))
lines(0:13, c(0,0,0,0,0,0,0,0,0,0,0,0,0,0),type='l',lty=2)