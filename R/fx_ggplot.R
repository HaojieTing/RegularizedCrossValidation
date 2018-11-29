multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
approx_fx1<-function(x){ n=40; parms =parmmat[which(parmmat$type=='ldnooutlierRegr_40_3'),]; (parms$sigma * x + parms$ omega * x*(x-1)+parms$gamma*(n/2-x)**2+parms$tau1 *x * (n/2-x)+parms$tau2 *x * (n/2-x))*4/n**2}
approx_fx2<-function(x){ n=40; parms =parmmat[which(parmmat$type=='ldnooutlierRegr_40_10'),]; (parms$sigma * x + parms$ omega * x*(x-1)+parms$gamma*(n/2-x)**2+parms$tau1 *x * (n/2-x)+parms$tau2 *x * (n/2-x))*4/n**2}
approx_fx3<-function(x){ n=80; parms =parmmat[which(parmmat$type=='ldnooutlierRegr_80_3'),]; (parms$sigma * x + parms$ omega * x*(x-1)+parms$gamma*(n/2-x)**2+parms$tau1 *x * (n/2-x)+parms$tau2 *x * (n/2-x))*4/n**2}

dirresult<-"F:/MyPaper/paper/BMTWOCV/results/"
parmmat<-read.csv(file=paste(dirresult,"parms.txt",sep=""))
LWNO40<-as.matrix(read.csv(file=paste(dirresult, "fxregr40nooutlier.txt", sep=""), header=FALSE))
LWNO80<-as.matrix(read.csv(file=paste(dirresult, "fxregr80nooutlier.txt", sep=""), header=FALSE) )
LWO40<-as.matrix(read.csv(file=paste(dirresult, "fxregr40outlier.txt", sep=""), header=FALSE))
LWO80<-as.matrix(read.csv(file=paste(dirresult, "fxregr80outlier.txt", sep=""), header=FALSE))
HD40<-as.matrix(read.csv(file=paste(dirresult, "fxhdregr40.txt", sep=""), header=FALSE))
HD80<-as.matrix(read.csv(file=paste(dirresult, "fxhdregr80.txt", sep=""), header=FALSE))

n=40
parms<-parmmat[which(parmmat$type=='ldnooutlierRegr_40_3'),]
c1 <- stat_function(fun=approx_fx1)
data_40_3<- (rev(LWNO40[1,]) +rev(LWNO40[2,])+LWNO40[3,]+LWNO40[4,])/4
p1 <- qplot(c(0:20), data_40_3, color=6, xlab='x', ylab="f(x)")+ theme(legend.position = "none")+geom_line() +  ggtitle("n=40, p=3")+c1
parms<-parmmat[which(parmmat$type=='ldnooutlierRegr_40_10'),]
c2 <- stat_function(fun=approx_fx2)
data_40_10<- (rev(LWNO40[6,]) +rev(LWNO40[7,])+LWNO40[8,]+LWNO40[9,])/4
p2 <- qplot(c(0:20), data_40_10, color=6, xlab='x', ylab="f(x)")+ theme(legend.position = "none")+geom_line() +  ggtitle("n=40, p=10")+c2

n=80
parms<-parmmat[which(parmmat$type=='ldnooutlierRegr_80_3'),]
c3 <- stat_function(fun=approx_fx3)
data_80_3<- (rev(LWNO80[1,]) +rev(LWNO80[2,])+LWNO80[3,]+LWNO80[4,])/4
p3 <- qplot(c(0:40), data_80_3, color=6, xlab='x', ylab="f(x)")+ theme(legend.position = "none")+geom_line() +  ggtitle("n=80, p=3")+c3



multiplot(p1,p1,p1,p2,p1,p1,p3,p1,p1,p1,p1,p1,cols=4)