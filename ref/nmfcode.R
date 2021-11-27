library("R.matlab")
#训练集
datatr <- readMat("USPStrainingdata.mat")
class(datatr)
length(datatr)

#测试集
datate <- readMat("USPStestingdata.mat")
class(datate)
length(datate)

#数字特征 list 由三个元素组成 
#第一个list对象比较有迷惑性
class(datatr[[1]])
dim(datatr[[1]])
datatr[[1]]

class(datatr[[2]])
dim(datatr[[2]])
datatr[[2]][1,]

class(datatr[[3]])
dim(datatr[[3]])
datatr[[3]][1:5,]

#变量特征存在第二个list中
feattr=datatr[[2]]

#代表的数字存在第三个list中
class(datatr[[3]])
dim(datatr[[3]])

#样本的类别归属 训练集标签处理
datatrnum1=datatr[[3]]
head(datatrnum1)
datatrnum=matrix(0,nrow(datatrnum1),1)
ntr=nrow(datatrnum1)#训练集样本个数
#代表真实数字
for(i in 1:ntr)
{
  datatrnum[i,1]=which(datatrnum1[i,]==1)-1
}

#测试集标签处理
featte=datate[[2]]
datatenum1=datate[[3]]
head(datatenum1)
datatenum=matrix(0,nrow(datatenum1),1)
nte=nrow(datatenum1)#测试集样本个数
#代表真实数字
for(i in 1:nte)
{
  datatenum[i,1]=which(datatenum1[i,]==1)-1
}


#标准化的范围
range(feattr)

#利用rgb取色 下面函数中的取色器
rgb(0.1,1,1,max=255)
rgb(1,1,1,max=255)

#画一个数字示意图
plotnum=function(feature)
{
  numma=feature
  numrgb=numma*127.5+127.5   #所有数值转成256级灰度，反变换
  rgbx=rep(c(1:16),16)       #每个图片256，转成16*16的图片
  rgby=NULL
  for(i in 16:1)
  {
    rgby=c(rgby,rep(i,16))
  }

  #生成rbg颜色
  rgbcolor=NULL
  for(i in 1:256)
  {
    r1=numrgb[i]
    g1=numrgb[i]
    b1=numrgb[i]
    rgbcolor=c(rgbcolor,rgb(r1,g1,b1,max=255)) 
  }
  png('./temp.png')
  plot(rgbx,rgby,col=rgbcolor,type="p",pch=15,cex=3)
  dev.off()
}

#数据前9个数及其对应图
which(datatrnum[1:100]==8)
plotnum(feattr[1,])#6
plotnum(feattr[2,])#5
plotnum(feattr[3,])#4
plotnum(feattr[4,])#7
plotnum(feattr[5,])#3
plotnum(feattr[8,])#1
plotnum(feattr[9,])#0
plotnum(feattr[42,])#2
plotnum(feattr[18,])#8
plotnum(feattr[65,])#9

#数据描述性分析
table(datatrnum)
barplot(table(datatrnum),col=heat.colors(10),main="训练集不同数字样本个数")

barplot(table(datatenum),col=heat.colors(10),main="测试集不同数字样本个数")


##非负矩阵分解

#特征值
dim(feattr)
fvalue=eigen(t(feattr)%*%feattr)
fvalue[[1]]

#计算主成分分析的累积贡献率
exprate=rep(0,256)
for(i in 1:256)
{
  exprate[i]=sum(fvalue[[1]][1:i])/sum(fvalue[[1]])
}

exprate

#非负矩阵分解
#install.packages("NMFN")
library("NMFN")
dim(feattr)
#将矩阵先变为原始的灰度表示
feattrp=feattr*127.5+127.5
#127.5=(max(x)-min(x))/2  max(x)=255,min(x)=0

datanmf=nnmf(t(feattrp),12)
nmfw=datanmf[[1]]#基矩阵
nmfh=datanmf[[2]]#系数矩阵
dim(nmfw)
dim(nmfh)

#接下来我们来看基矩阵
#观测12个基向量是由原始哪一些变量解释的
plotvar=function(nmfwma,plotname)
{
  nmfwmasc=(nmfwma-min(nmfwma))/(max(nmfwma)-min(nmfwma))#标准化到0~1
  nmfw255=nmfwmasc*255#变成rbg颜色
  rgbx=rep(c(1:16),16)
  rgby=NULL
  for(i in 16:1)
  {
    rgby=c(rgby,rep(i,16))
  }
  
  #生成rbg颜色
  rgbcolor=NULL
  for(i in 1:256)
  {
    r1=nmfw255[i]
    g1=nmfw255[i]
    b1=nmfw255[i]
    rgbcolor=c(rgbcolor,rgb(r1,g1,b1,max=255)) 
  }
  plot(rgbx,rgby,col=rgbcolor,type="p",pch=15,cex=3,
  main=plotname)
}
plotvar(nmfw[,1],"第1个新变量灰度图")
plotvar(nmfw[,2],"第2个新变量灰度图")
plotvar(nmfw[,3],"第3个新变量灰度图")
plotvar(nmfw[,4],"第4个新变量灰度图")
plotvar(nmfw[,5],"第5个新变量灰度图")
plotvar(nmfw[,6],"第6个新变量灰度图")
plotvar(nmfw[,7],"第7个新变量灰度图")
plotvar(nmfw[,8],"第8个新变量灰度图")
plotvar(nmfw[,9],"第9个新变量灰度图")
plotvar(nmfw[,10],"第10个新变量灰度图")
plotvar(nmfw[,11],"第11个新变量灰度图")
plotvar(nmfw[,12],"第12个新变量灰度图")

#分析h矩阵
dim(nmfh)
#转置该矩阵，让其便于与因变量拼接
nmfht=t(nmfh)
nmfhy=cbind(nmfht,datatrnum)#合并后矩阵



#write.csv(nmfw,"12维的基矩阵.csv",row.names=F)
#write.csv(nmfh,"12维的系数矩阵.csv",row.names=F)

#
round(nmfhy[5,],3)

#排序
#install.packages("plyr")
library(plyr)
nmfhy=as.data.frame(nmfhy)
head(nmfhy)  #得到所有训练集样本在12个新变量上的系数

dim(nmfhy)  #7291*13

#看每个新变量在哪些数字上的权重更大


var1=arrange(aggregate(V1~V13,nmfhy,mean),desc(V1))#排序第1个变量
var1
#按照第13个变量去汇总统计nmfhy中的v1值，并进行平均
var2=arrange(aggregate(V2~V13,nmfhy,mean),desc(V2))#排序第2个变量

var3=arrange(aggregate(V3~V13,nmfhy,mean),desc(V3))#排序第3个变量

var4=arrange(aggregate(V4~V13,nmfhy,mean),desc(V4))#排序第4个变量

var5=arrange(aggregate(V5~V13,nmfhy,mean),desc(V5))#排序第5个变量

var6=arrange(aggregate(V6~V13,nmfhy,mean),desc(V6))#排序第6个变量

var7=arrange(aggregate(V7~V13,nmfhy,mean),desc(V7))#排序第7个变量

var8=arrange(aggregate(V8~V13,nmfhy,mean),desc(V8))#排序第8个变量

var9=arrange(aggregate(V9~V13,nmfhy,mean),desc(V9))#排序第9个变量

var10=arrange(aggregate(V10~V13,nmfhy,mean),desc(V10))#排序第10个变量

var11=arrange(aggregate(V11~V13,nmfhy,mean),desc(V11))#排序第11个变量

var12=arrange(aggregate(V12~V13,nmfhy,mean),desc(V12))#排序第12个变量

write.csv(cbind(var1,var8,var10),"12不同数字不同新变量中的权重.csv")

###########################################

#模型

#训练集
feattr=as.data.frame(feattr)
feattrda=cbind(feattr*127.5+127.5,datatrnum)
dim(feattrda)
head(feattrda)
names(feattrda)[257]="num"

#测试集

featte=as.data.frame(featte)
featteda=cbind(featte*127.5+127.5,datatenum)
head(featteda)
names(featteda)[257]="num"
dim(featteda)

?scale
#神经网络
library(nnet)
#标准化
#标准化函数
scale01=function(x)
{
  xscale=(x-min(x))/(max(x)-min(x))
  return(xscale)
}


a=apply(feattrda[,1:256],2,scale01)

datatrsc=cbind(apply(feattrda[,1:256],2,scale01),feattrda[,257])
datatesc=cbind(apply(featteda[,1:256],2,scale01),featteda[,257])

datatrsc=as.data.frame(datatrsc)
names(datatrsc)[257]="y"
datatrsc$y=as.factor(datatrsc$y)

datatesc=as.data.frame(datatesc)
names(datatesc)[257]="y"
datatesc$y=as.factor(datatesc$y)

head(datatrsc)


#确定初始权重
rang1=1/max(abs(datatrsc[,1:256]))
#模型,选择隐层
corrate=rep(0,10)
for(i in 1:10)
{
  set.seed(1)
  nn1=nnet(y~.,datatrsc,size=i,rang=rang1)
  #预测
  prenn1=predict(nn1,datatesc,type="class")
  #正确率
  corrate[i]=sum(prenn1==datatesc[,257])/nrow(datatesc)
}
nn1=nnet(y~.,datatrsc,size=1,rang=rang1)


#随机森林
library(randomForest)
#随机森林预测
feattrda$num=as.factor(feattrda$num)
featteda$num=as.factor(featteda$num)


#测试集数字索引记录
tr0=(datatenum==0)
tr1=(datatenum==1)
tr2=(datatenum==2)
tr3=(datatenum==3)
tr4=(datatenum==4)
tr5=(datatenum==5)
tr6=(datatenum==6)
tr7=(datatenum==7)
tr8=(datatenum==8)
tr9=(datatenum==9)

difnumtot=matrix(0,1,10)
rf1=randomForest(num~.,feattrda,mtry=16,ntree=100,importance=T)
prerf1=predict(rf1,featteda,type="class")
#总体预测正确率
sum(prerf1==featteda[,257])/nrow(featteda)
#不同数字正确率
difnumtot[,1]=sum(prerf1[tr0]==featteda[tr0,257])/sum(tr0)
difnumtot[,2]=sum(prerf1[tr1]==featteda[tr1,257])/sum(tr1)
difnumtot[,3]=sum(prerf1[tr2]==featteda[tr2,257])/sum(tr2)
difnumtot[,4]=sum(prerf1[tr3]==featteda[tr3,257])/sum(tr3)
difnumtot[,5]=sum(prerf1[tr4]==featteda[tr4,257])/sum(tr4)
difnumtot[,6]=sum(prerf1[tr5]==featteda[tr5,257])/sum(tr5)
difnumtot[,7]=sum(prerf1[tr6]==featteda[tr6,257])/sum(tr6)
difnumtot[,8]=sum(prerf1[tr7]==featteda[tr7,257])/sum(tr7)
difnumtot[,9]=sum(prerf1[tr8]==featteda[tr8,257])/sum(tr8)
difnumtot[,10]=sum(prerf1[tr9]==featteda[tr9,257])/sum(tr9)
#write.csv(difnumtot,"原数据不同数字预测正确率.csv")

difnumtot=as.vector(difnumtot)
names(difnumtot)=c(0:9)
barplot(difnumtot,col=heat.colors(10)[1:10],main="原数据不同数字预测正确率",
xlab="数字",ylab="正确率")





#用非负矩阵分解的预测
head(feattrda)
datatot=rbind(feattrda,featteda)

nmftot=nnmf(t(datatot[,1:256]),12)
nmfwtot=nmftot[[1]]#基矩阵
nmfhtot=nmftot[[2]]#系数矩阵
#nmf随机森林
#nmf
dim(nmfhtot)
nmfdata=as.data.frame(t(nmfhtot))
nmfdatay=cbind(nmfdata,datatot[,257])
head(nmfdatay)
dim(nmfdatay)
names(nmfdatay)[13]="num"
nmfdatay$num=as.factor(nmfdatay$num)
#随机森林
nrow(feattrda)
trnum=c(1:7291)
tenum=c(7292:9298)
rfnmf=randomForest(num~.,nmfdatay[trnum,],mtry=4,ntree=100)
#预测
prenmf=predict(rfnmf,nmfdatay[tenum,],type="class")
sum(prenmf==nmfdatay[tenum,13])/length(tenum)

############################################
#降低维数与预测正确率。
rfrate=rep(0,15)
for(i in 1:15)
{

  datatot=rbind(feattrda,featteda)

  nmftot=nnmf(t(datatot[,1:256]),i)
  nmfwtot=nmftot[[1]]#基矩阵
  nmfhtot=nmftot[[2]]#系数矩阵
  #nmf随机森林
  #nmf

  nmfdata=as.data.frame(t(nmfhtot))
  nmfdatay=cbind(nmfdata,datatot[,257])

  names(nmfdatay)[i+1]="num"
  nmfdatay$num=as.factor(nmfdatay$num)
  #随机森林
  rfnmf=randomForest(num~.,nmfdatay[trnum,],mtry=ceiling(sqrt(i)),ntree=100)
  #预测
  prenmf=predict(rfnmf,nmfdatay[tenum,],type="class")
  rfrate[i]=sum(prenmf==nmfdatay[tenum,i+1])/length(tenum)
}
#write.csv(as.matrix(rfrate),"rfrate.csv")
rfrate
#主成分分析

head(datatot)
#主成分
numpca=princomp(datatot[,1:256],cor=TRUE)
pcasum=summary(numpca,loadings=TRUE)
names(pcasum)
pcasum$loadings[,1]

dim(datatot[,1:256])
datatot1=as.matrix(datatot[,1:256])
head(datatot1)

corpca=rep(0,15)
for(i in 1:15)
{
  datapca=datatot1%*%as.matrix(pcasum$loadings[,1:i])
  datapca1=as.data.frame(cbind(datapca,datatot[,257]))
  head(datapca1)
  names(datapca1)[i+1]="num"
  datapca1$num=as.factor(datapca1$num)
  #随机森林
  rfpca=randomForest(num~.,datapca1[trnum,],mtry=ceiling(sqrt(i)),ntree=100)
  prepca=predict(rfpca,datapca1[tenum,],type="class")
  corpca[i]=sum(prepca==datapca1$num[tenum])/length(tenum)
  
}


#画图
plot(corpca,type="b",col="lightblue",pch=1,xlab="维数",ylab="准确率",
main="PCA及NMF在不同维数预测的正确率线图",ylim=c(0,1))
lines(rfrate,type="b",col="pink",pch=2)
legend("bottomright",inset=0.05,title="PCA and NMF",c("PCA","NMF"),
lty=c(1,1),pch=c(1,2),col=c("lightblue","pink"))

#write.csv(matrix(c(rfrate,corpca),15,2),"rate.csv")
a=read.csv("rate.csv")
corpca=a[,3]
rfrate=a[,2]

#####################################################
#####################################################
#去掉某个变量后的预测效果

#用非负矩阵分解的预测
head(feattrda)
datatot=rbind(feattrda,featteda)

nmftot=nnmf(t(datatot[,1:256]),12)
nmfwtot=nmftot[[1]]#基矩阵
nmfhtot=nmftot[[2]]#系数矩阵
#nmf随机森林
#nmf
dim(nmfhtot)
nmfdata=as.data.frame(t(nmfhtot))
nmfdatay=cbind(nmfdata,datatot[,257])
head(nmfdatay)
dim(nmfdatay)
names(nmfdatay)[13]="num"
nmfdatay$num=as.factor(nmfdatay$num)


#由于0、1差别最大，找出代表0和1的变量
#找出0，1的权重
weight01=matrix(0,12,2)#第一列储存0的值，第二列储存1的值
for(i in 1:12)
{
  aggre01=aggregate(nmfdatay[,i]~nmfdatay[,13],FUN=mean)
  weight01[i,1]=aggre01[1,2]
  weight01[i,2]=aggre01[2,2]
}

colnames(weight01)=c("zero","one")
#找出权重差距满足一定阈值的
therehold=5
index01=which(weight01[,1]/weight01[,2]>therehold)#0大于1
index10=which(weight01[,2]/weight01[,1]>therehold)#1大于0




#非上述变量中所预测的数字差距较大的
index01t=c(index01,index10)
#未被选入的变量
indexd=c(1:12)[-index01t]
indexlist=vector("list",length(indexd))#记录前三和后三的数字
for(i in 1:length(indexd))
{
  aggret=aggregate(nmfdatay[,indexd[i]]~nmfdatay[,13],FUN=mean)
  names(aggret)=c("num","meanvalue")
  indexlist[[i]]=arrange(aggret,desc(meanvalue))[c(1:3,8:10),]
  names(indexlist)[i]=indexd[i]
}



#随机森林

trnum=c(1:7291)
tenum=c(7292:9298)
#将倍数从大到小排列的索引
orin=order(-c((weight01[,1]/weight01[,2])[index01],(weight01[,2]/weight01[,1])[index10]))
index01t
#随机森林变量个数
mtry=ceiling(sqrt(12-length(index01t)))
#预测
names(nmfdatay)[13]="num"
rateall=rep(0,length(index01t))
rate01=rep(0,length(index01t))

nmfdataytr=nmfdatay[tenum,]
nmfdatayte=nmfdatay[trnum,]

#0、1部分
pre01=(nmfdatayte[,13]==0|nmfdatayte[,13]==1)
#根据列表选出最大的是2
pre2=(nmfdatayte[,13]==2)
rate2=rep(0,length(index01t))

for(i in 1:length(index01t))
{
  exvar=index01t[orin][1:i]
  rfnmf1=randomForest(num~.,data=nmfdataytr[,c(1:13)[-exvar]],mtry=mtry,ntree=100)
  prerfsub=predict(rfnmf1,nmfdatayte[,c(1:13)[-exvar]],type="class")
  rateall[i]=sum(prerfsub==nmfdatayte[,13])/nrow(nmfdatayte)
  rate01[i]=sum(prerfsub[pre01]==nmfdatayte[pre01,13])/sum(pre01)
  rate2[i]=sum(prerfsub[pre2]==nmfdatayte[pre2,13])/sum(pre2)
}
plot(rate01,type="b",col="red",pch=1,xlab="去除变量个数",ylab="正确率",
main="去掉与0、1相关变量预测的正确率变化线图",ylim=c(0.5,1))
lines(rate2,type="b",col="blue",pch=2)

legend("bottomleft",inset=0.05,title="number",c("0、1","2"),
lty=c(1,1),pch=c(1,2),col=c("red","blue"))
prema=matrix(0,length(index01t),2)
prema[,1]=rate01
prema[,2]=rate2
#write.csv(prema,"去掉与0、1相关变量预测的正确率变化线图9.csv")


par(mfrow=c(1,2))
#只看0、1
#按照0、1排序结果
ma01=arrange(as.data.frame(cbind(c(1:12),weight01)),desc(zero))
index00=ma01[,1]
#
head(nmfdataytr)

prerm0=matrix(0,11,2)
pre0=(nmfdatayte[,13]==0)
pre1=(nmfdatayte[,13]==1)
for(i in 1:11)
{
  exvar=index00[1:i]
  rf01=randomForest(num~.,data=nmfdataytr[,c(1:13)[-exvar]],
  mtry=ceiling(sqrt(12-i)),ntree=100)
  prerf1=predict(rf01,nmfdatayte[,c(1:13)[-exvar]],type="class")
  prerm0[i,1]=sum(prerf1[pre0]==nmfdatayte[pre0,13])/sum(pre0)
  prerm0[i,2]=sum(prerf1[pre1]==nmfdatayte[pre1,13])/sum(pre1)
}
plot(prerm0[,1],type="b",col="red",pch=1,xlab="去除变量个数",ylab="正确率",
main="去掉与0相关变量预测的正确率变化线图",ylim=c(0,1))
lines(prerm0[,2],type="b",col="blue",pch=2)
legend("bottomleft",inset=0.05,title="different number",c("0","1"),
lty=c(1,1),pch=c(1,2),col=c("red","blue"))
#write.csv(prerm0,"去掉与0相关变量预测的正确率变化线图.csv")

#按照1、0排序结果
ma10=arrange(as.data.frame(cbind(c(1:12),weight01)),desc(one))
index11=ma10[,1]
#
head(nmfdataytr)
prerm1=matrix(0,11,2)
pre0=(nmfdatayte[,13]==0)
pre1=(nmfdatayte[,13]==1)
for(i in 1:11)
{
  exvar=index11[1:i]
  rf10=randomForest(num~.,data=nmfdataytr[,c(1:13)[-exvar]],
  mtry=ceiling(sqrt(12-i)),ntree=100)
  prerf0=predict(rf10,nmfdatayte[,c(1:13)[-exvar]],type="class")
  prerm1[i,1]=sum(prerf0[pre0]==nmfdatayte[pre0,13])/sum(pre0)
  prerm1[i,2]=sum(prerf0[pre1]==nmfdatayte[pre1,13])/sum(pre1)
}
plot(prerm1[,1],type="b",col="red",pch=1,xlab="去除变量个数",ylab="正确率",
main="去掉与1相关变量预测的正确率变化线图",ylim=c(0,1))
lines(prerm1[,2],type="b",col="blue",pch=2)
legend("bottomleft",inset=0.05,title="number",c("0","1"),
lty=c(1,1),pch=c(1,2),col=c("red","blue"))
#write.csv(prerm1,"去掉与1相关变量预测的正确率变化线图.csv")




#不同数字在不同维度下正确率的变化，取15维。
rfrate=rep(0,15)
#测试集数字索引记录
tr0=(datatenum==0)
tr1=(datatenum==1)
tr2=(datatenum==2)
tr3=(datatenum==3)
tr4=(datatenum==4)
tr5=(datatenum==5)
tr6=(datatenum==6)
tr7=(datatenum==7)
tr8=(datatenum==8)
tr9=(datatenum==9)
#记录不同数字正确率
difnum=matrix(0,15,10)
for(i in 1:15)
{

  datatot=rbind(feattrda,featteda)

  nmftot=nnmf(t(datatot[,1:256]),i)
  nmfwtot=nmftot[[1]]#基矩阵
  nmfhtot=nmftot[[2]]#系数矩阵
  #nmf随机森林
  #nmf

  nmfdata=as.data.frame(t(nmfhtot))
  nmfdatay=cbind(nmfdata,datatot[,257])

  names(nmfdatay)[i+1]="num"
  nmfdatay$num=as.factor(nmfdatay$num)
  #随机森林
  nmfdatatr1=nmfdatay[trnum,]
  nmfdatate1=nmfdatay[tenum,]
  rfnmf=randomForest(num~.,nmfdatatr1,mtry=ceiling(sqrt(i)),ntree=100)
  #预测
  prenmf=predict(rfnmf,nmfdatate1,type="class")
  rfrate[i]=sum(prenmf==nmfdatate1[,i+1])/length(tenum)
  difnum[i,1]=sum(prenmf[tr0]==nmfdatate1[tr0,i+1])/sum(tr0)
  difnum[i,2]=sum(prenmf[tr1]==nmfdatate1[tr1,i+1])/sum(tr1)
  difnum[i,3]=sum(prenmf[tr2]==nmfdatate1[tr2,i+1])/sum(tr2)
  difnum[i,4]=sum(prenmf[tr3]==nmfdatate1[tr3,i+1])/sum(tr3)
  difnum[i,5]=sum(prenmf[tr4]==nmfdatate1[tr4,i+1])/sum(tr4)
  difnum[i,6]=sum(prenmf[tr5]==nmfdatate1[tr5,i+1])/sum(tr5)
  difnum[i,7]=sum(prenmf[tr6]==nmfdatate1[tr6,i+1])/sum(tr6)
  difnum[i,8]=sum(prenmf[tr7]==nmfdatate1[tr7,i+1])/sum(tr7)
  difnum[i,9]=sum(prenmf[tr8]==nmfdatate1[tr8,i+1])/sum(tr8)
  difnum[i,10]=sum(prenmf[tr9]==nmfdatate1[tr9,i+1])/sum(tr9)
}
#write.csv(difnum,"不同数字不同维度预测的正确率.csv")
plot(difnum[,1],type="b",pch=1,xlab="维度",ylab="预测正确率",
main="NMF不同数字不同降维度的预测正确率",ylim=c(0,1),col=rainbow(10)[1])
for(i in 2:10)
{
  lines(difnum[,i],type="b",pch=i,col=rainbow(10)[i])
}
lines(rfrate,type="b",pch=11,col="black")
legend("bottomright",inset=0.05,title="different number",
c("0","1","2","3","4","5","6","7","8","9","total"),
lty=rep(1,11),pch=c(1:11),
col=c(rainbow(10),"black"))
#write.csv(rfrate,"随机森林不同维度预测正确率.csv")
table(nmfdatate1$num,prenmf)
#


#################################################

#主成分不同数字预测的正确率
numpca=princomp(datatot[,1:256],cor=TRUE)
pcasum=summary(numpca,loadings=TRUE)
names(pcasum)
pcasum$loadings[,1]

dim(datatot[,1:256])
datatot1=as.matrix(datatot[,1:256])
head(datatot1)

corpca=rep(0,15)
difnumpca=matrix(0,15,10)

for(i in 1:15)
{
  datapca=datatot1%*%as.matrix(pcasum$loadings[,1:i])
  datapca1=as.data.frame(cbind(datapca,datatot[,257]))
  names(datapca1)[i+1]="num"
  datapca1$num=as.factor(datapca1$num)
  
  datapcatr=datapca1[trnum,]
  datapcate=datapca1[tenum,]
  #随机森林
  rfpca=randomForest(num~.,datapcatr,mtry=ceiling(sqrt(i)),ntree=100)
  prepca=predict(rfpca,datapcate,type="class")
  
  corpca[i]=sum(prepca==datapcate$num)/length(tenum)

  difnumpca[i,1]=sum(prepca[tr0]==datapcate[tr0,i+1])/sum(tr0)
  difnumpca[i,2]=sum(prepca[tr1]==datapcate[tr1,i+1])/sum(tr1)
  difnumpca[i,3]=sum(prepca[tr2]==datapcate[tr2,i+1])/sum(tr2)
  difnumpca[i,4]=sum(prepca[tr3]==datapcate[tr3,i+1])/sum(tr3)
  difnumpca[i,5]=sum(prepca[tr4]==datapcate[tr4,i+1])/sum(tr4)
  difnumpca[i,6]=sum(prepca[tr5]==datapcate[tr5,i+1])/sum(tr5)
  difnumpca[i,7]=sum(prepca[tr6]==datapcate[tr6,i+1])/sum(tr6)
  difnumpca[i,8]=sum(prepca[tr7]==datapcate[tr7,i+1])/sum(tr7)
  difnumpca[i,9]=sum(prepca[tr8]==datapcate[tr8,i+1])/sum(tr8)
  difnumpca[i,10]=sum(prepca[tr9]==datapcate[tr9,i+1])/sum(tr9)
  
}


#write.csv(difnumpca,"PCA不同数字不同维度预测的正确率.csv")

plot(difnumpca[,1],type="b",pch=1,xlab="维度",ylab="预测正确率",
main="PCA不同数字不同降维度的预测正确率",ylim=c(0,1),col=rainbow(10)[1])
for(i in 2:10)
{
  lines(difnumpca[,i],type="b",pch=i,col=rainbow(10)[i])
}
lines(corpca,type="b",pch=11,col="black")
legend("bottomright",inset=0.05,title="different number",
c("0","1","2","3","4","5","6","7","8","9","total"),
lty=rep(1,11),pch=c(1:11),
col=c(rainbow(10),"black"))
#write.csv(corpca,"PCA随机森林不同维度预测正确率")
table(nmfdatate1$num,prenmf)




#PCA及NMF不同维度与原数据不同数字预测正确率

par(mfrow=c(1,2))

#PCA

plot(difnumtot,type="b",pch=1,xlab="数字",ylab="预测正确率",
main="PCA不同数字不同降维度的预测正确率",ylim=c(0,1),col=rainbow(5)[1]
,xaxt="n")
axis(1,at=c(1:10),label=c(0:9))
lines(c(1:10),difnumpca[12,],type="b",pch=2,col=rainbow(5)[2])
lines(c(1:10),difnumpca[9,],type="b",pch=3,col=rainbow(5)[3])
lines(c(1:10),difnumpca[6,],type="b",pch=4,col=rainbow(5)[4])
lines(c(1:10),difnumpca[3,],type="b",pch=5,col=rainbow(5)[5])

legend("bottomright",inset=0.05,title="different dimensionality",
c("255","12","9","6","3"),
lty=rep(1,5),pch=c(1:5),
col=rainbow(5))

#NMF
plot(difnumtot,type="b",pch=1,xlab="数字",ylab="预测正确率",
main="NMF不同数字不同降维度的预测正确率",ylim=c(0,1),col=rainbow(5)[1]
,xaxt="n")
axis(1,at=c(1:10),label=c(0:9))
lines(c(1:10),difnum[12,],type="b",pch=2,col=rainbow(5)[2])
lines(c(1:10),difnum[9,],type="b",pch=3,col=rainbow(5)[3])
lines(c(1:10),difnum[6,],type="b",pch=4,col=rainbow(5)[4])
lines(c(1:10),difnum[3,],type="b",pch=5,col=rainbow(5)[5])

legend("bottomright",inset=0.05,title="different dimensionality",
c("255","12","9","6","3"),
lty=rep(1,5),pch=c(1:5),
col=rainbow(5))




#更容易把哪些数字预测成哪些数字
#取6维
nmftot6=nnmf(t(datatot[,1:256]),6)
nmfwtot6=nmftot6[[1]]#基矩阵
nmfhtot6=nmftot6[[2]]#系数矩阵
#nmf随机森林
#nmf

nmfdata6=as.data.frame(t(nmfhtot6))
nmfdatay6=cbind(nmfdata6,datatot[,257])

names(nmfdatay6)[6+1]="num"
nmfdatay6$num=as.factor(nmfdatay6$num)
#随机森林
nmfdatatr6=nmfdatay6[trnum,]
nmfdatate6=nmfdatay6[tenum,]
rfnmf6=randomForest(num~.,nmfdatatr6,mtry=ceiling(sqrt(6)),ntree=100)
prenmf6=predict(rfnmf6,nmfdatate6,type="class")
sum(prenmf6==nmfdatate6$num)/length(tenum)
table6=table(nmfdatate6$num,prenmf6)
#write.csv(as.matrix(table6),"取6维把哪些数字预测错.csv")
plot(c(1,2),c(9,10),type="b",col=rgb(100,255,255,max=255))



#颜色越深代表预测错的个数越多，形状越大。
plotpre=function(actma,prema,mainname,adjust)
{
  actma[actma==0]=10
  prema1=as.numeric(as.character(prema))
  prema1[prema1==0]=10
  tablei=table(actma,prema1)
  rgbx=rep(c(1:10),10)
  rgby=NULL
  for(i in 1:10)
  {
    rgby=c(rgby,rep(i,10))
  }

  for(j in 1:10){tablei[j,j]=0}#主对角线线变成0,主对角线是预测正确
  maxrbg=max(tablei)
  shapematrix=tablei#形状矩阵
  colormatrix=maxrbg-tablei#颜色矩阵    
  
  #生成rbg颜色
  rgbcolor=NULL
  for(i in 1:10)
  {
    for(j in 1:10)
    {
      rgbcolor=c(rgbcolor,rgb(maxrbg,maxrbg,colormatrix[i,j],max=maxrbg)) 
    }

  }
  shapevector=as.vector(t(shapematrix))
  plot(rgbx,rgby,col=rgbcolor,type="p",pch=16,cex=shapevector/adjust,
  main=mainname,
  xlab="预测数字",ylab="原数字",xaxt="n",yaxt="n")
  text(rgbx,rgby,shapevector,col="lightblue")
  axis(1,at=c(1:10),label=c(1:9,0))
  axis(2,at=c(1:10),label=c(1:9,0))
}
par(mfrow=c(1,2))
plotpre(datatenum,prenmf6,"NMF维度是6的数字错判图",3)



#更容易把哪些数字预测成哪些数字
#取3维
nmftot3=nnmf(t(datatot[,1:256]),3)
nmfwtot3=nmftot3[[1]]#基矩阵
nmfhtot3=nmftot3[[2]]#系数矩阵
#nmf随机森林
#nmf

nmfdata3=as.data.frame(t(nmfhtot3))
nmfdatay3=cbind(nmfdata3,datatot[,257])

names(nmfdatay3)[3+1]="num"
nmfdatay3$num=as.factor(nmfdatay3$num)
#随机森林
nmfdatatr3=nmfdatay3[trnum,]
nmfdatate3=nmfdatay3[tenum,]
rfnmf3=randomForest(num~.,nmfdatatr3,mtry=ceiling(sqrt(3)),ntree=100)
prenmf3=predict(rfnmf3,nmfdatate3,type="class")
sum(prenmf3==nmfdatate3$num)/length(tenum)
table3=table(nmfdatate3$num,prenmf3)
#write.csv(as.matrix(table3),"取3维把哪些数字预测错.csv")
plotpre(datatenum,prenmf3,"NMF维度是3的数字错判图",5)


table(datatenum,prenmf3)

######pca
#6维
numpca=princomp(datatot[,1:256],cor=TRUE)
pcasum=summary(numpca,loadings=TRUE)
names(pcasum)
pcasum$loadings[,1]

dim(datatot[,1:256])
datatot1=as.matrix(datatot[,1:256])
head(datatot1)

datapca=datatot1%*%as.matrix(pcasum$loadings[,1:6])
datapca1=as.data.frame(cbind(datapca,datatot[,257]))
names(datapca1)[6+1]="num"
datapca1$num=as.factor(datapca1$num)
  
datapcatr=datapca1[trnum,]
datapcate=datapca1[tenum,]
#随机森林
rfpca6=randomForest(num~.,datapcatr,mtry=ceiling(sqrt(6)),ntree=100)
prepca6=predict(rfpca6,datapcate,type="class")

plotpre(datatenum,prepca6,"PCA维度是6的数字错判图",3)


#3维
datapca=datatot1%*%as.matrix(pcasum$loadings[,1:3])
datapca1=as.data.frame(cbind(datapca,datatot[,257]))
names(datapca1)[3+1]="num"
datapca1$num=as.factor(datapca1$num)
  
datapcatr=datapca1[trnum,]
datapcate=datapca1[tenum,]
#随机森林
rfpca3=randomForest(num~.,datapcatr,mtry=ceiling(sqrt(3)),ntree=100)
prepca3=predict(rfpca3,datapcate,type="class")
plotpre(datatenum,prepca3,"PCA维度是3的数字错判图",5)




#总体
plotpre(datatenum,prerf1,"原数据预测数字错判图",2)


#错判数字图像

#4到9
pre49=which((datatenum==4&prerf1==9))
preo4=which(datatenum==4&prerf1==4)
#预测错误的4
plotnum(featte[pre49[1],])
plotnum(featte[pre49[2],])
plotnum(featte[pre49[6],])
#预测正确的4
plotnum(featte[preo4[1],])
plotnum(featte[preo4[2],])
plotnum(featte[preo4[3],])



#3到5

pre35=which((datatenum==3&prerf1==5))
preo3=which(datatenum==3&prerf1==3)
#预测错误的3
plotnum(featte[pre35[1],])
plotnum(featte[pre35[2],])
plotnum(featte[pre35[6],])
#预测正确的3
plotnum(featte[preo3[1],])
plotnum(featte[preo3[2],])
plotnum(featte[preo3[3],])

#5到3

pre53=which((datatenum==5&prerf1==3))
preo5=which(datatenum==5&prerf1==5)
#预测错误的5
plotnum(featte[pre53[1],])
plotnum(featte[pre53[2],])
plotnum(featte[pre53[6],])
#预测正确的5
plotnum(featte[preo5[1],])
plotnum(featte[preo5[2],])
plotnum(featte[preo5[3],])



#1维矩阵
datanmf1=nnmf(t(feattrp),1)
nmfw1=datanmf1[[1]]#基矩阵
nmfh1=datanmf1[[2]]#系数矩阵


#观测新变量是由原始哪一些变量解释的
plotvar(nmfw1[,1],"一维基矩阵图新变量1")

nmfht1=t(nmfh1)
nmfhy1=cbind(nmfht1,datatrnum)#合并后矩阵

#排序
nmfhy1=as.data.frame(nmfhy1)

matrix1=arrange(aggregate(V1~V2,nmfhy1,mean),desc(V1))#排序第1个变量
#write.csv(matrix1,"降维到一维不同数字平均权重.csv")

#2维矩阵
datanmf2=nnmf(t(feattrp),2)
nmfw2=datanmf2[[1]]#基矩阵
nmfh2=datanmf2[[2]]#系数矩阵

#观测新变量是由原始哪一些变量解释的
plotvar(nmfw2[,1],"二维基矩阵图新变量1")
plotvar(nmfw2[,2],"二维基矩阵图新变量2")

nmfht2=t(nmfh2)
nmfhy2=cbind(nmfht2,datatrnum)#合并后矩阵

#排序
nmfhy2=as.data.frame(nmfhy2)
head(nmfhy2)
matrix21=arrange(aggregate(V1~V3,nmfhy2,mean),desc(V1))#排序第1个变量
matrix22=arrange(aggregate(V2~V3,nmfhy2,mean),desc(V2))#排序第1个变量
#write.csv(matrix21,"降维到二维新变量1不同数字平均权重.csv")
#write.csv(matrix22,"降维到二维新变量2不同数字平均权重.csv")


#3维矩阵
datanmf3=nnmf(t(feattrp),3)
nmfw3=datanmf3[[1]]#基矩阵
nmfh3=datanmf3[[2]]#系数矩阵

#观测新变量是由原始哪一些变量解释的
plotvar(nmfw3[,1],"三维基矩阵图新变量1")
plotvar(nmfw3[,2],"三维基矩阵图新变量2")
plotvar(nmfw3[,3],"三维基矩阵图新变量3")
which(datatrnum==6)


nmfht3=t(nmfh3)
nmfhy3=cbind(nmfht3,datatrnum)#合并后矩阵

#排序
nmfhy3=as.data.frame(nmfhy3)

matrix31=arrange(aggregate(V1~V4,nmfhy3,mean),desc(V1))#排序第1个变量
matrix32=arrange(aggregate(V2~V4,nmfhy3,mean),desc(V2))#排序第2个变量
matrix33=arrange(aggregate(V3~V4,nmfhy3,mean),desc(V3))#排序第3个变量
#write.csv(matrix31,"降维到三维新变量1不同数字平均权重.csv")
#write.csv(matrix32,"降维到三维新变量2不同数字平均权重.csv")
#write.csv(matrix33,"降维到三维新变量3不同数字平均权重.csv")

#实际结果及分解后结果
which(datatrnum==1)
plotvar(feattr[32,],"实际数字6")
plotvar(nmfw3%*%nmfh3[,32],"降维到三维数字6")

plotvar(feattr[8,],"实际数字1")
plotvar(nmfw3%*%nmfh3[,8],"降维到三维数字1")

#12维实际结果及分解后结果

datanmf=nnmf(t(feattrp),12)
nmfw=datanmf[[1]]#基矩阵
nmfh=datanmf[[2]]#系数矩阵
dim(nmfw)
dim(nmfh)

which(datatrnum==6)
datatrnum[32,]
plotvar(feattr[32,],"实际数字6")
plotvar(nmfw%*%nmfh[,32],"降维到12维数字6")


datatrnum[11,]
plotvar(feattr[18,],"实际数字8")
plotvar(nmfw%*%nmfh[,18],"降维到12维数字8")

#数字8的权重
weight8=nmfh[,18]
write.csv(weight8,"降维到12维数字8的权重.csv")

