scale01=function(x)
{
  xscale=(x-min(x))/(max(x)-min(x))
  return(xscale)
}


neural = function(train, test)
{
  trainScaledX = apply(train[['x']], 2, scale01)
  testScaledX = apply(test[['x']], 2, scale01)

  trainScaled = data.frame(trainScaledX, train[['y']])
  testScaled = data.frame(testScaledX, test[['y']])

  names(trainScaled)[ncol(trainScaled)] = "y"
  names(testScaled)[ncol(testScaled)] = "y"

  trainScaled$y = as.factor(trainScaled$y)
  testScaled$y = as.factor(testScaled$y)

  rang1 = 1/max(abs(trainScaledX))
  corrate=rep(0,10)
  for(i in 1:10)
  {
    set.seed(1)
    nn1=nnet(y ~.,trainScaled,size=i,rang=rang1, MaxNWts=30000)
    #预测
    prenn1=predict(nn1,testScaled,type="class")
    #正确率
    corrate[i]=sum(prenn1==testScaled$y)/nrow(testScaled)
  }
  corrateNNET = data.frame(size = 1: 10, corrate_rate = corrate)
  write.csv(corrateNNET, "../output/corrateNNET.csv", row.names = FALSE)
}


forest = function(train, test, classNames)
{
  feattrda = data.frame(train[['x']], num = train[['y']])
  featteda = data.frame(test[['x']], num = test[['y']])
  feattrda$num=as.factor(feattrda$num)
  featteda$num=as.factor(featteda$num)

  difnumtot=matrix(0,1,10)
  # rf1=randomForest(num~.,feattrda,mtry=16,ntree=100,importance=T)
  rf1=randomForest(num~.,feattrda,mtry=16,ntree=10,importance=T)
  prerf1=predict(rf1,featteda,type="class")
  #总体预测正确率
  print(sum(prerf1==featteda[,ncol(featteda)])/nrow(featteda))
  #不同数字正确率
  for(i in 1:10)
  {
    tr0=(featteda$num==(i-1))
    difnumtot[,i]=sum(prerf1[tr0]==featteda[tr0,ncol(featteda)])/sum(tr0)
  }
  write.csv(difnumtot,"../output/随机森林不同类别预测正确率.csv")

  difnumtot=as.vector(difnumtot)
  names(difnumtot) = classNames
  pdf('../figure/随机森林不同类别预测正确率.pdf', family = "GB1", width = 10, height = 5)
  barplot(difnumtot,col=heat.colors(10)[1:10], xlab="种类",ylab="正确率")
  dev.off()
}


forestDimNMF = function(train, test)
{
  feattrda = data.frame(train[['x']], num = train[['y']])
  featteda = data.frame(test[['x']], num = test[['y']])
  feattrda$num=as.factor(feattrda$num)
  featteda$num=as.factor(featteda$num)
  datatot=rbind(feattrda,featteda)
  trnum = c(1:nrow(feattrda))
  tenum = c(1:nrow(featteda))

  rfrate=rep(0,15)
  for(i in 1:15)
  {
    nmftot=nnmf(t(datatot[,1:ncol(datatot) - 1]),i)
    nmfwtot=nmftot[[1]]#基矩阵
    nmfhtot=nmftot[[2]]#系数矩阵
    nmfdata=as.data.frame(t(nmfhtot))
    nmfdatay=cbind(nmfdata,datatot[, ncol(datatot)])
    names(nmfdatay)[i+1]="num"
    nmfdatay$num=as.factor(nmfdatay$num)
    #随机森林
    # rfnmf=randomForest(num~.,nmfdatay[trnum,],mtry=ceiling(sqrt(i)),ntree=100)
    rfnmf=randomForest(num~.,nmfdatay[trnum,],mtry=ceiling(sqrt(i)),ntree=10)
    prenmf=predict(rfnmf,nmfdatay[tenum,],type="class")
    rfrate[i]=sum(prenmf==nmfdatay[tenum,i+1])/length(tenum)
  }
  correctRFdimNMF = data.frame(dim = 1: 15, correct_rate = rfrate)
  write.csv(correctRFdimNMF, "../output/correctRFdimNMF.csv", row.names = FALSE)
}


forestDimPCA = function(train, test)
{
  feattrda = data.frame(train[['x']], num = train[['y']])
  featteda = data.frame(test[['x']], num = test[['y']])
  feattrda$num=as.factor(feattrda$num)
  featteda$num=as.factor(featteda$num)
  datatot=rbind(feattrda,featteda)
  trnum = c(1:nrow(feattrda))
  tenum = c(1:nrow(featteda))

  numpca=princomp(datatot[,1: (ncol(datatot) - 1)],cor=TRUE)
  pcasum=summary(numpca,loadings=TRUE)
  pcasum$loadings[,1]
  datatot1=as.matrix(datatot[,1: (ncol(datatot) - 1)])
  corpca=rep(0,15)
  for(i in 1:15)
  {
    datapca=datatot1%*%as.matrix(pcasum$loadings[,1:i])
    datapca1=as.data.frame(cbind(datapca,datatot[,ncol(datatot)]))
    names(datapca1)[i+1]="num"
    datapca1$num=as.factor(datapca1$num)
    #随机森林
    # rfpca=randomForest(num~.,datapca1[trnum,],mtry=ceiling(sqrt(i)),ntree=100)
    rfpca=randomForest(num~.,datapca1[trnum,],mtry=ceiling(sqrt(i)),ntree=10)
    prepca=predict(rfpca,datapca1[tenum,],type="class")
    corpca[i]=sum(prepca==datapca1$num[tenum])/length(tenum)
  }
  correctRFdimPCA = data.frame(dim = 1: 15, correct_rate = corpca)
  write.csv(correctRFdimPCA, "../output/correctRFdimPCA.csv", row.names = FALSE)
}


forestDimClassNMF = function(train, test, classNames)
{
  feattrda = data.frame(train[['x']], num = train[['y']])
  featteda = data.frame(test[['x']], num = test[['y']])
  feattrda$num=as.factor(feattrda$num)
  featteda$num=as.factor(featteda$num)
  datatot=rbind(feattrda,featteda)
  trnum = c(1:nrow(feattrda))
  tenum = c(1:nrow(featteda))

  rfrate=rep(0,15)
  difnum=matrix(0,15,10)
  for(i in 1:15)
  {
    datatot=rbind(feattrda,featteda)
    nmftot=nnmf(t(datatot[,1:(ncol(datatot) - 1)]),i)
    nmfwtot=nmftot[[1]]#基矩阵
    nmfhtot=nmftot[[2]]#系数矩阵
    nmfdata=as.data.frame(t(nmfhtot))
    nmfdatay=cbind(nmfdata,datatot[,ncol(datatot)])
    names(nmfdatay)[i+1]="num"
    nmfdatay$num=as.factor(nmfdatay$num)
    #随机森林
    nmfdatatr1=nmfdatay[trnum,]
    nmfdatate1=nmfdatay[tenum,]
    # rfnmf=randomForest(num~.,nmfdatatr1,mtry=ceiling(sqrt(i)),ntree=100)
    rfnmf=randomForest(num~.,nmfdatatr1,mtry=ceiling(sqrt(i)),ntree=10)
    #预测
    prenmf=predict(rfnmf,nmfdatate1,type="class")
    rfrate[i]=sum(prenmf==nmfdatate1[,i+1])/length(tenum)
    for(j in 1:10)
    {
      tr0=(featteda$num==(j - 1))
      difnum[i,j]=sum(prenmf[tr0]==nmfdatate1[tr0,i+1])/sum(tr0)
    }
  }
  pdf('../figure/NMF不同类别不同降维度的预测正确率.pdf', family = "GB1", width = 8, height = 5)
  plot(difnum[,1],type="b",pch=1,xlab="维度",ylab="正确率",
       ylim=c(0,1),col=rainbow(10)[1])
  for(i in 2:10)
  {
    lines(difnum[,i],type="b",pch=i,col=rainbow(10)[i])
  }
  lines(rfrate,type="b",pch=11,col="black")
  legend("bottomright",inset=0.05,title="类别",
  c(classNames, "total"),
  lty=rep(1,11),pch=c(1:11),
  col=c(rainbow(10),"black"))
  dev.off()
}


forestDimClassPCA = function(train, test, classNames)
{
  feattrda = data.frame(train[['x']], num = train[['y']])
  featteda = data.frame(test[['x']], num = test[['y']])
  feattrda$num=as.factor(feattrda$num)
  featteda$num=as.factor(featteda$num)
  datatot=rbind(feattrda,featteda)
  trnum = c(1:nrow(feattrda))
  tenum = c(1:nrow(featteda))

  numpca=princomp(datatot[,1: (ncol(datatot) - 1)],cor=TRUE)
  pcasum=summary(numpca,loadings=TRUE)
  pcasum$loadings[,1]
  datatot1=as.matrix(datatot[,1:(ncol(datatot) - 1)])
  corpca=rep(0,15)
  difnumpca=matrix(0,15,10)
  for(i in 1:15)
  {
    datapca=datatot1%*%as.matrix(pcasum$loadings[,1:i])
    datapca1=as.data.frame(cbind(datapca,datatot[,ncol(datatot)]))
    names(datapca1)[i+1]="num"
    datapca1$num=as.factor(datapca1$num)
    datapcatr=datapca1[trnum,]
    datapcate=datapca1[tenum,]
    #随机森林
    # rfpca=randomForest(num~.,datapcatr,mtry=ceiling(sqrt(i)),ntree=100)
    rfpca=randomForest(num~.,datapcatr,mtry=ceiling(sqrt(i)),ntree=10)
    prepca=predict(rfpca,datapcate,type="class")
    corpca[i]=sum(prepca==datapcate$num)/length(tenum)

    for(j in 1:10)
    {
      tr0=(featteda$num==(j - 1))
      difnumpca[i,j]=sum(prepca[tr0]==datapcate[tr0,i+1])/sum(tr0)
    }
  }
  pdf('../figure/PCA不同类别不同降维度的预测正确率.pdf', family = "GB1", width = 8, height = 5)
  plot(difnumpca[,1],type="b",pch=1,xlab="维度",ylab="正确率",
       ylim=c(0,1),col=rainbow(10)[1])
  for(i in 2:10)
  {
    lines(difnumpca[,i],type="b",pch=i,col=rainbow(10)[i])
  }
  lines(corpca,type="b",pch=11,col="black")
  legend("bottomright",inset=0.05,title="类别",
  c(classNames, "total"),
  lty=rep(1,11),pch=c(1:11),
  col=c(rainbow(10),"black"))
  dev.off()
}


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


misJudge= function(train, test, classNames, dimension)
{

  feattrda = data.frame(train[['x']], num = train[['y']])
  featteda = data.frame(test[['x']], num = test[['y']])
  feattrda$num=as.factor(feattrda$num)
  featteda$num=as.factor(featteda$num)
  datatot=rbind(feattrda,featteda)
  trnum = c(1:nrow(feattrda))
  tenum = c(1:nrow(featteda))
  datatrnum = feattrda$num
  datatenum = featteda$num

  nmftot6=nnmf(t(datatot[,1: (ncol(datatot) - 1)]), dimension)
  nmfwtot6=nmftot6[[1]]#基矩阵
  nmfhtot6=nmftot6[[2]]#系数矩阵

  nmfdata6=as.data.frame(t(nmfhtot6))
  nmfdatay6=cbind(nmfdata6,datatot[, ncol(datatot)])

  names(nmfdatay6)[dimentsion + 1]="num"
  nmfdatay6$num=as.factor(nmfdatay6$num)
  #随机森林
  nmfdatatr6=nmfdatay6[trnum,]
  nmfdatate6=nmfdatay6[tenum,]
  # rfnmf6=randomForest(num~.,nmfdatatr6,mtry=ceiling(sqrt(6)),ntree=100)
  rfnmf6=randomForest(num~.,nmfdatatr6,mtry=ceiling(sqrt(6)),ntree=10)
  prenmf6=predict(rfnmf6,nmfdatate6,type="class")
  sum(prenmf6==nmfdatate6$num)/length(tenum)
  table6=table(nmfdatate6$num,prenmf6)

  plotname = paste0('../figure/', dimension, "维NMF降维随机森林错判图.pdf")
  pdf(plotname, family = "GB1", width = 10, height = 10)
  plotpre(datatenum,prenmf6, plotname, 25, classNames)
  dev.off()
}

