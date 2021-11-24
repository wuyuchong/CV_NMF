plotRaw = function(feature)
{
  numrgb=feature
  rgbx=rep(c(1:28),28)       #每个图片256，转成28*28的图片
  rgby=NULL
  for(i in 28:1)
  {
    rgby=c(rgby,rep(i,28))
  }
  #生成rbg颜色
  rgbcolor=NULL
  for(i in 1:784)
  {
    r1=numrgb[i]
    g1=numrgb[i]
    b1=numrgb[i]
    rgbcolor=c(rgbcolor,rgb(r1,g1,b1,max=255)) 
  }
  pdf('../figure/raw.pdf')
  plot(rgbx,rgby,col=rgbcolor,type="p",pch=15,cex=3,xlab="",ylab="")
  dev.off()
}

plotVar=function(nmfw)
{
  pdf('../figure/var.pdf')
  par(mfrow = c(3, 4))
  for(i in 1: ncol(nmfw))
  {
    nmfwma = nmfw[, i]
    plotname = paste("Var:", i)
    nmfwmasc=(nmfwma-min(nmfwma))/(max(nmfwma)-min(nmfwma))#标准化到0~1
    nmfw255=nmfwmasc*255#变成rbg颜色
    rgbx=rep(c(1:28),28)
    rgby=NULL
    for(i in 28:1)
    {
      rgby=c(rgby,rep(i,28))
    }
    rgbcolor=NULL # 生成rbg颜色
    for(i in 1:784)
    {
      r1=nmfw255[i]
      g1=nmfw255[i]
      b1=nmfw255[i]
      rgbcolor=c(rgbcolor,rgb(r1,g1,b1,max=255)) 
    }
    plot(rgbx,rgby,col=rgbcolor,type="p",pch=15,cex=3, main=plotname,xlab="",ylab="")
  }
  dev.off()
}

plotNMFforestPCA = function()
{
  rfrate = read.csv("../output/correctRFdimNMF.csv")$correct_rate
  corpca = read.csv("../output/correctRFdimPCA.csv")$correct_rate
  pdf('../figure/PCA及NMF在不同维数预测的正确率线图.pdf')
  # par(family = "SimHei")
  plot(corpca,type="b",col="lightblue",pch=1,xlab="dimensions",ylab="correct rate", ,ylim=c(0,1))
  lines(rfrate,type="b",col="pink",pch=2)
  legend("bottomright",inset=0.05,title="PCA and NMF",c("PCA","NMF"),
  lty=c(1,1),pch=c(1,2),col=c("lightblue","pink"))
  dev.off()
}
