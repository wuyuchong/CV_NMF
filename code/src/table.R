tableWeight = function(nmfh, train, labels)
{
  nmfht=t(nmfh)
  nmfhy=cbind(nmfht, train[['y']])#合并后矩阵
  nmfhy=as.data.frame(nmfhy)

  var1=arrange(aggregate(V1~V13,nmfhy,mean),desc(V1))#排序第1个变量

  var1=arrange(aggregate(V1~V13,nmfhy,mean),desc(V1))#排序第1个变量

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

  raw = data.frame(var1[, 1], var2[, 1], var3[, 1], var4[, 1], var5[, 1], var6[, 1])
  table = data.frame(权重排名 = 1:10)
  for(i in 1:ncol(raw))
  {
    co = data.frame(raw[, i])
    names(co) = 'label'
    co = dplyr::left_join(co, labels, by = "label")
    co = data.frame(co[, 2])
    names(co) = paste("变量", i)
    table = cbind(table, co)
  }
  write.csv(table, "../output/10种不同类别在前六个新变量中的权重.csv", row.names = FALSE)
}
