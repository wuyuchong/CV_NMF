# ------------> 加载程序包
library(NMFN)
library(forcats)
library(plyr)
library(nnet)
library(randomForest)


# ------------> 加载依赖函数
source("src/load.R")
source("src/plot.R")
source("src/table.R")
source("src/model.R")


# ------------> 加载数据集
load_mnist()
classNames = c("T恤/上衣" , "裤子" , "套头衫" , "连衣裙" , "外套" , "凉鞋" , "衬衫" , "运动鞋" , "包" , "短靴")


# ------------> 标准化: -1 -- 1
train[['Xscaled']] = (train[['x']] - 127.5) / 127.5
test[['Xscaled']] = (test[['x']] - 127.5) / 127.5

feattrda = data.frame(train[['x']], num = train[['y']])
featteda = data.frame(test[['x']], num = test[['y']])
feattrda$num=as.factor(feattrda$num)
featteda$num=as.factor(featteda$num)
datatot=rbind(feattrda,featteda)
trnum = c(1:nrow(feattrda))
tenum = c(1:nrow(featteda))
datatrnum = feattrda$num
datatenum = featteda$num

#更容易把哪些数字预测成哪些数字
#取6维
nmftot6=nnmf(t(datatot[,1: (ncol(datatot) - 1)]),6)
nmfwtot6=nmftot6[[1]]#基矩阵
nmfhtot6=nmftot6[[2]]#系数矩阵
#nmf随机森林
#nmf

nmfdata6=as.data.frame(t(nmfhtot6))
nmfdatay6=cbind(nmfdata6,datatot[, ncol(datatot)])

names(nmfdatay6)[6+1]="num"
nmfdatay6$num=as.factor(nmfdatay6$num)
#随机森林
nmfdatatr6=nmfdatay6[trnum,]
nmfdatate6=nmfdatay6[tenum,]
# rfnmf6=randomForest(num~.,nmfdatatr6,mtry=ceiling(sqrt(6)),ntree=100)
rfnmf6=randomForest(num~.,nmfdatatr6,mtry=ceiling(sqrt(6)),ntree=10)
prenmf6=predict(rfnmf6,nmfdatate6,type="class")
sum(prenmf6==nmfdatate6$num)/length(tenum)
table6=table(nmfdatate6$num,prenmf6)
#write.csv(as.matrix(table6),"取6维把哪些数字预测错.csv")
# plot(c(1,2),c(9,10),type="b",col=rgb(100,255,255,max=255))

#颜色越深代表预测错的个数越多，形状越大。
plotpre=function(actma,prema,mainname,adjust, classNames)
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
       xlab="预测值",ylab="真实值",xaxt="n",yaxt="n", las = 2)
  text(rgbx,rgby,shapevector,col="lightblue")
  # axis(1,at=c(1:10),label=c(1:9,0))
  axis(1,at=c(1:10),label = classNames)
  # axis(2,at=c(1:10),label=c(1:9,0))
  axis(2,at=c(1:10),label = classNames)
}

pdf('../figure/NMF维度是6的类别错判图.pdf', family = "GB1", width = 10, height = 10)
plotpre(datatenum,prenmf6,"NMF维度是6的数字错判图", 25, classNames)
dev.off()
