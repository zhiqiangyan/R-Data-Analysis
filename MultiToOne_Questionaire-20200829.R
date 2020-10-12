#���ܣ�һ�������˶���ʾ��ǣ�������id��������
#��־ǿ��20200829

## 1 ���ζ�ȡ�����ļ�
#ɾ����ǰ���ж���
rm(list=ls(all=TRUE))

#���Ĳ���·��
route <- "C:/Users/yanzq/Desktop/"
setwd(route)

#��ȡcsv�ļ�
fl1 <- read.csv("test1.csv",header=TRUE) #���� 28��
fl2 <- read.csv("test2.csv",header=TRUE) #���� 20��
fl3 <- read.csv("test3.csv",header=TRUE) #���� 20��
fl4 <- read.csv("test4.csv",header=TRUE) #ִ�й��ܺ������������� 40��

#summary(fl)
#names(fl) #��ȡ���������ݱ�����
#str(fl) #�鿴���ݽṹ

## 2 ��ȡ�ļ��е�id����Ϣ����id���󲢼�
id_1 <- union(fl1$id, fl2$id)
id_2 <- union(fl3$id, fl4$id)
id_u<- union(id_1, id_2)

## 3 ����id����Ϣ�����ļ��е���Ϣ��������
# �½����ݿ�
fl <- data.frame(id=id_1)

# �ļ����ݶԱ�
library(dplyr)
fl <- left_join(fl, fl1, by = "id") #������룬��id��Ϊ��׼�����ұߵ����ݶ��뵽���
fl <- left_join(fl, fl2, by = "id")
fl <- left_join(fl, fl3, by = "id")
fl <- left_join(fl, fl4, by = "id")

## 4 ����ļ�
write.table(fl,"fl.csv",row.names=FALSE,col.names=TRUE,sep=",")