#功能：一个人填了多份问卷星，根据其id进行整合
#颜志强，20200829

## 1 依次读取数据文件
#删除当前所有对象
rm(list=ls(all=TRUE))

#更改操作路径
route <- "C:/Users/yanzq/Desktop/"
setwd(route)

#读取csv文件
fl1 <- read.csv("test1.csv",header=TRUE) #共情 28题
fl2 <- read.csv("test2.csv",header=TRUE) #抑郁 20题
fl3 <- read.csv("test3.csv",header=TRUE) #焦虑 20题
fl4 <- read.csv("test4.csv",header=TRUE) #执行功能和情绪调节困难 40题

#summary(fl)
#names(fl) #获取完整的数据变量名
#str(fl) #查看数据结构

## 2 获取文件中的id列信息，对id列求并集
id_1 <- union(fl1$id, fl2$id)
id_2 <- union(fl3$id, fl4$id)
id_u<- union(id_1, id_2)

## 3 根据id列信息索求文件中的信息进行整合
# 新建数据框
fl <- data.frame(id=id_1)

# 文件数据对标
library(dplyr)
fl <- left_join(fl, fl1, by = "id") #向左对齐，以id列为标准，将右边的数据对齐到左边
fl <- left_join(fl, fl2, by = "id")
fl <- left_join(fl, fl3, by = "id")
fl <- left_join(fl, fl4, by = "id")

## 4 输出文件
write.table(fl,"fl.csv",row.names=FALSE,col.names=TRUE,sep=",")