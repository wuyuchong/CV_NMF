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
  print(rang1)
  corrate=rep(0,10)
  for(i in 1:10)
  {
    set.seed(1)
    nn1=nnet(y ~.,trainScaled,size=i,rang=rang1)
    #预测
    prenn1=predict(nn1,testScaled,type="class")
    #正确率
    corrate[i]=sum(prenn1==testScaled$y)/nrow(testScaled)
  }
  corrateNNET = data.frame(size = rep(0, 10), corrate_rate = corrate)
  write.csv(corrateNNET, "../output/corrateNNET.csv", row.names = FALSE)
}


forest = function(train, test)
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
  names(difnumtot)=c(0:9)
  pdf('../figure/随机森林不同类别预测正确率.pdf')
  barplot(difnumtot,col=heat.colors(10)[1:10],main="随机森林不同类别预测正确率",
  xlab="类别",ylab="正确率")
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
  write.csv(correctRFdimPCA, "../output/correctRFdimNMF.csv", row.names = FALSE)
}

