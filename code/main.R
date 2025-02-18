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
# classNames = c('T-shirt/top', 'Trouser', 'Pullover', 'Dress', 'Coat', 'Sandal', 'Shirt', 'Sneaker', 'Bag', 'Ankle boot')
classNames = c("T恤/上衣" , "裤子" , "套头衫" , "连衣裙" , "外套" , "凉鞋" , "衬衫" , "运动鞋" , "包" , "短靴")
labels = data.frame(label = 0:9, classNames = classNames)


# ------------> 标准化: -1 -- 1
train[['Xscaled']] = (train[['x']] - 127.5) / 127.5
test[['Xscaled']] = (test[['x']] - 127.5) / 127.5


# ------------> 绘图：原数据集
plotRaw(train[['x']][1, ])


# ------------> 非负矩阵分解: 求解
datanmf = nnmf(t(train[['x']]),12)
save(datanmf, file = '../model/datanmf.RData')
load("../model/datanmf.RData")
nmfw = datanmf[[1]]#基矩阵
nmfh = datanmf[[2]]#系数矩阵


# ------------> 绘图：降维后的变量图
plotVar(nmfw)


# ------------> 表格：降维后类别的权重
tableWeight(nmfh, train, labels)


# ------------> 神经网络
neural(train, test)


# ------------> 随机森林
forest(train, test, classNames)


# ------------> NMF / PCA - 维度 - 随机森林
# NMF
forestDimNMF(train, test)
# PCA
forestDimPCA(train, test)
# plot
plotNMFforestPCA()


# ------------> NMF / PCA - 维度 - 种类 - 随机森林
# NMF
forestDimClassNMF(train, test, classNames)
# PCA
forestDimClassPCA(train, test, classNames)


# ------------> NMF - 随机森林 - 错判图
misJudge(train, test, classNames, 1)


