library(MASS)##加载用到的程序包
library(rpart)
library(class)
library(igraph)
library(kknn)
library(e1071)
library(supclust)
library(PearsonDS)

P1TrueValue<-0.711099658276
R1TrueValue<-0.694109088

d<-5  #样本空间维数
n<-50   #样本量
M<-1000 #重复次数
f<-5

F1TrueValueVector<-rep(P1TrueValue,M)
F2TrueValueVector<-rep(R1TrueValue,M)
num<-0   #混淆矩阵对角元素有0的次数
count<-1 #重复次数的初值
mean1<-rep(0,d)
cov1<-diag(d)
mean2<-rep(1,d)
cov2<-2*cov1


lambada<-1##F值的先验分布参数
alph<-c(0.025,0.975)##显著性水平

#——————————————————————实验中用到的函数————————
#随机抽取h个样本
GSample<-function(h){
  hh<-trunc(h/2)
  o<-sample(h,hh)
  x<-matrix(NA,h,d)
  y<-rep(NA,h)
  x[o,]<-mvrnorm(hh,mean1,cov1)
  x[-o,]<-mvrnorm(h-hh,mean2,cov2)
  y[o]<-0
  y[-o]<-1
  return(list(x,y))
}

#########################
###找出每类的下标
PerClassIndexs<-function(y)
{
  a<-vector("list")
  n<-length(y)
  PerClassNums<-table(y)#每类的个数
  ClassNames<-attributes(PerClassNums)$dimnames[1]$y##每一类的类名
  for(i in 1:length(PerClassNums))
  {
    a[[i]]<-which(y==ClassNames[i])
  }
  return(a)
}

############################
#多类分f折函数（分层分组）
#y为类别变量，classindex为类别的下标（向量）,f为折数
############################
PGroups<-function(y,f)
{ 
  f<-trunc(f)
  n<-length(y)
  groups<-vector("list",f)
  
  a<-PerClassIndexs(y)
  classindex<-IndexSample(a)
  
  if(n!=length(classindex))
    stop("length of y must be equal to length of classindex")
  if(f<2)
    stop(" f should be greater than or equal to 2")
  else if (f>n)
    stop("f should be less than or equal to the number of observations")
  else if(f==n)
    groups<-c(1:n)
  else 
  {
    for(i in 1:n)
    {
      jj<-trunc((i-1)%%f)+1
      cnt<-trunc((i-1)/f)+1
      groups[[jj]][cnt]<-classindex[i]
    }
  }
  return(groups)
}

###########################
#随机排列下标函数：a为每类的下标（类型为列表）
###########################
IndexSample<-function(a)
{ if(length(a[[1]])==1)
  index<-as.vector(a[[1]])
  else index<-sample(as.vector(a[[1]]))
  for(i in 2:length(a))
  {
    if(length(a[[i]])==1) 
      index<-c(index,as.vector(a[[i]]))
    else  index<-c(index,sample(as.vector(a[[i]])))
  }
  return(index)
}

##f折交叉验证(分层)拟合与预测，分类算法CART(输出混淆矩阵和混淆矩阵对角元素，为了以后判别F值存在于否)
CrossvalStr_R_fFold<-function(x,y,f)
{
  f<-trunc(f)
  y<-factor(y)
  x<-data.frame(x)
  datat<-cbind(x,y)
  groups<-PGroups(y,f)##数据按类别分层分为f折
  
  ##f折交叉验证拟与预测，输出混淆矩阵和混淆矩阵对角元素
  y<-as.numeric(as.vector(y))
  ConfuseMatrix<-array(NA,dim=c(2,2,f))
  diagConfuseAll<-rep(NA,2*f)
  
  for(i in 1:f)
  {
    fit1<-rpart(y~.,data=datat[-groups[[i]],])
    pre1<-as.numeric(as.vector(predict(fit1,datat[groups[[i]],],type="class")))
    
    ConfuseMatrix[,,i]<-table(y[groups[[i]]],pre1)
    diagConfuseAll[(2*(i-1)+1):(2*i)]<-diag(ConfuseMatrix[,,i])
  }
  return(list(ConfuseMatrix=ConfuseMatrix,diagConfuseAll=diagConfuseAll))
}
#根据混淆矩阵计算F值的函数FBasedOnConM
FBasedOnConM<-function(conMat,lambada)
{
  E1<-(conMat[1,1]+lambada)/(conMat[1,1]+conMat[2,1]+2*lambada)
  V1<-(conMat[1,1]+lambada)*(conMat[2,1]+lambada)/(conMat[1,1]+conMat[2,1]+2*lambada)**2/(conMat[1,1]+conMat[2,1]+2*lambada+1)
  
  P1<-(conMat[1,1])/(conMat[1,1]+conMat[2,1])
  R1<-(conMat[1,1])/(conMat[1,1]+conMat[1,2])
  
  
  EV<-rbind(E1,V1,P1,R1)
  return(EV)
}

###用分布求分位数
###cmatrix是混淆矩阵，alph为显著性水平（可以同时求多个显著性水平下的分位数）
FQuantileBPrimeTwoClass<-function(lambada,cmatrix,alph)
{
  cmatrix<-cmatrix/f
  FQB1<-rep(NA,length(alph))
  FQB2<-FQB1
  
  tp1<-cmatrix[1,1]
  fp1<-cmatrix[2,1]
  fn1<-cmatrix[1,2]
  
  tp2<-cmatrix[2,2]
  
  for(i in 1:length(alph))
  {
    al<-alph[i]
    FQB1[i]<-qbeta(al,tp1+lambada,fp1+lambada)
    FQB2[i]<-qbeta(al,tp1+lambada,fn1+lambada)
  }
  
  FQB<-rbind(FQB1,FQB2)
  return(FQB)
}

FQuantileBetaTwoClass<-function(a1,b1,alph)
{
  FQB<-rep(NA,length(alph))
  
  
  for(i in 1:length(alph))
  {
    a<-alph[i]
    FQB[i]<-qbeta(a,a1,b1)
    
  }
  
  return(FQB)
}


###看某值是否在某区间内（值可以是向量，区间为对应的向量）
FInOrNotInteval<-function(Fvalue,LeftInterval,RigthInterval)
{
  if(length(Fvalue)!=length(LeftInterval)||length(Fvalue)!=length(RigthInterval))
    stop("值与对应的区间个数应该相等")
  FvalueInOr<-rep(NA,length(Fvalue))
  InCount<-0##计算包含在区间内的个数
  for(fv in 1:length(Fvalue))
  {
    if(Fvalue[fv]>=LeftInterval[fv]&& Fvalue[fv]<=RigthInterval[fv])
    {
      FvalueInOr[fv]<-1
      InCount<-InCount+1
    }
    else FvalueInOr[fv]<-0
    
  }
  return(list(InCount,FvalueInOr))
}

ConF_5fold<-vector("list",6)

for(j in 1:5)
{
  ConF_5fold[[j]]<-matrix(NA,2*M,2)
}
ConF_5fold[[6]]<-matrix(0,2*M,2)


f_5F1AN<-matrix(NA,M,5)
f_5F2AN<-f_5F1AN
f_5F3AN<-f_5F1AN

f_5a1<-vector()
f_5b1<-f_5a1

f_5p<-f_5a1

f_5p_mic<-matrix(NA,M,1)
f_5r_mic<-matrix(NA,M,1)
f_5p_micc<-f_5a1
f_5r_micc<-f_5a1


a1<-f_5a1
b1<-f_5a1


#重复M次交叉验证
print(date())
while(TRUE)
{
  #产生样本
  data_all<-GSample(n)
  x<-data_all[[1]]
  y<-data_all[[2]]
  
  y<-factor(y)
  x<-data.frame(x)
  
  ###循环中一些中间变量，用于存储混淆矩阵，便利于计算P,R,F值
  
  result_5fold<-CrossvalStr_R_fFold(x,y,5)
  
  
  
  if(min(result_5fold$diagConfuseAll)>0)
  {
    if(count%%200==0)
      print(paste("第",count,"次实验"))
    
    
    for(k2 in 1:5)
    {
      ConF_5fold[[k2]][(2*count-1):(2*count),]<-result_5fold$ConfuseMatrix[,,k2]
      ConF_5fold[[6]][(2*count-1):(2*count),]<-ConF_5fold[[6]][(2*count-1):(2*count),]+ConF_5fold[[k2]][(2*count-1):(2*count),]
      ##分别计算出5cv的F1,F2值（一类和二类F值）
      f_5F12<-FBasedOnConM(ConF_5fold[[k2]][(2*count-1):(2*count),],lambada)
      f_5F1AN[count,k2]<-f_5F12[1]
      f_5F2AN[count,k2]<-f_5F12[2]
      
      f_5F3AN[count,k2]<-f_5F12[3]
      
    }
    f_5a1[count]<-mean(f_5F1AN[count,])
    f_5b1[count]<-sum(f_5F2AN[count,])/(f**2)
    a1[count]<-(f_5a1[count])*((f_5a1[count])-(f_5a1[count])**2-(f_5b1[count]))/(f_5b1[count])
    b1[count]<-(1-(f_5a1[count]))*((f_5a1[count])-(f_5a1[count])**2-(f_5b1[count]))/(f_5b1[count])
    
    
    f_5p[count]<-mean(f_5F3AN[count,])
    
    
    
    
    f_512<-FBasedOnConM(ConF_5fold[[6]][(2*count-1):(2*count),],lambada)
    
    f_5p_mic[count,1]<-f_512[3]
    f_5p_micc[count]<-mean(f_5p_mic[count,])
    f_5r_mic[count,1]<-f_512[4]
    f_5r_micc[count]<-mean(f_5r_mic[count,])
    
    
    
    count<-count+1##循环次数累加
  }
  else 
  {
    print(paste("第",count,"次实验混淆矩阵对角元素中有0"))
    num<-num+1
  }
  if(count>M)
    break
}

f_5FBP<-matrix(NA,M+1,9)

for(rpt in 1:M)
{ f_5FBP[rpt,c(1:2)]<-FQuantileBPrimeTwoClass(lambada, ConF_5fold[[6]][(2*rpt-1):(2*rpt),],alph)[1,]
  f_5FBP[rpt,c(4:5)]<-FQuantileBPrimeTwoClass(lambada, ConF_5fold[[6]][(2*rpt-1):(2*rpt),],alph)[2,]
  f_5FBP[rpt,c(7:8)]<-FQuantileBetaTwoClass(a1[rpt], b1[rpt],alph)
}

f_5FBP[1:M,3]<-f_5FBP[1:M,2]-f_5FBP[1:M,1]
f_5FBP[1:M,6]<-f_5FBP[1:M,5]-f_5FBP[1:M,4]
f_5FBP[1:M,9]<-f_5FBP[1:M,8]-f_5FBP[1:M,7]
f_5FBP[M+1,]<-colMeans(f_5FBP[1:M,])

CountFInRateBP<-matrix(0,1,3)

#5折cv
CountFInRateBP[1,1]<-FInOrNotInteval(F1TrueValueVector,f_5FBP[1:M,1],f_5FBP[1:M,2])[[1]]
CountFInRateBP[1,2]<-FInOrNotInteval(F2TrueValueVector,f_5FBP[1:M,4],f_5FBP[1:M,5])[[1]]
CountFInRateBP[1,3]<-FInOrNotInteval(F1TrueValueVector,f_5FBP[1:M,7],f_5FBP[1:M,8])[[1]]


#最后算出比率

CountFInRateBP<-CountFInRateBP


ConF_5foldFIN<-rbind(colMeans( ConF_5fold[[6]][seq(1,2*M,2),]),colMeans( ConF_5fold[[6]][seq(2,2*M,2),]))

f_5pmac<-mean(f_5p)
f_5pmic<-mean(f_5p_micc)
f_5rmic<-mean(f_5r_micc)

f_5pmacbias<-mean(f_5p-P1TrueValue)
f_5pmicbias<-mean(f_5p_micc-P1TrueValue)
f_5rmicbias<-mean(f_5r_micc-R1TrueValue)

f_5pmacMse<-mean((f_5p-P1TrueValue)**2)
f_5pmicMse<-mean((f_5p_micc-P1TrueValue)**2)
f_5rmicMse<-mean((f_5r_micc-R1TrueValue)**2)





CIfinal<-c(f_5FBP[M+1,1:9],as.vector(CountFInRateBP),f_5pmac,f_5pmic,f_5rmic,f_5pmacbias,f_5pmicbias,f_5rmicbias,f_5pmacMse,
           f_5pmicMse,f_5rmicMse)





names(CIfinal)<-c("PmicLeft","PmicRight","IL_Pmic","RmicLeft","RmicRight",
                  "IL_Rmic","PmacLeft","PmacRight","IL_Pmac",
                  "DOC_Pmic","DOC_Rmic","DOC_Pmac","Mean_Pmac","Mean_Pmic","Mean_Rmic","Bias_Pmac","Bias_Pmic","Bias_Rmic","Mse_Pmac","Mse_Pmic","Mse_Rmic")

filename2=paste("CART 分类 PR值的区间长度和置信度 ", as.character(n),"个 维数d=",
                as.character(d),"alph=",as.character(alph[2]),"mean1=",as.character(mean1[1]),"sigma1=",as.character(cov1[1]),
                "mean2=",as.character(mean2[1]),"sigma2=",as.character(cov2[1]),".csv")
#write.csv(CIfinal,filename2)

