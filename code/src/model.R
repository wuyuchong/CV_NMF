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
  sum(prerf1==featteda[,ncol(featteda)])/nrow(featteda)
  #不同数字正确率
  for(i in 1:10)
  {
    tr0=(datatenum==i)
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
