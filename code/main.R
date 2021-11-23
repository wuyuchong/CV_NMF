# ------------> 加载程序包
library(NMFN)
library(forcats)
library(plyr)
library(nnet)
library(randomForest)



# ------------> 加载函数
source("src/load.R")
source("src/plot.R")
source("src/table.R")
source("src/model.R")


# ------------> 加载数据集
load_mnist()
class_names = c('T-shirt/top', 'Trouser', 'Pullover', 'Dress', 'Coat', 'Sandal', 'Shirt', 'Sneaker', 'Bag', 'Ankle boot')


# ------------> 标准化: -1 -- 1
train[['Xscaled']] = (train[['x']] - 127.5) / 127.5
test[['Xscaled']] = (test[['x']] - 127.5) / 127.5


# ------------> 绘图：原数据集
plotRaw(train[['x']][1, ])


# ------------> 非负矩阵分解: 求解
# datanmf = nnmf(t(train[['x']]),12)
# save(datanmf, file = '../model/datanmf.RData')
load("../model/datanmf.RData")
nmfw = datanmf[[1]]#基矩阵
nmfh = datanmf[[2]]#系数矩阵


# ------------> 绘图：降维后的变量图
plotVar(nmfw)


# ------------> 表格：降维后类别的权重
tableWeight(nmfh, train)


# ------------> 神经网络
# neural(train, test)


# ------------> 随机森林
forest(train, test)





