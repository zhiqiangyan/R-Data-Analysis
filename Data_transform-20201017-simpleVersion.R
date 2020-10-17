#程序编写日期为20200404
#by zhiqiang yan

replace_empty_f <- function(fl){
  #读取csv文件	
  fl <- read.csv("10.12_data.csv",header=TRUE,sep=",",na.strings = "999")
  #获取行和列
  row <- dim(fl)[1]  
  col <- dim(fl)[2]
  
  for (i in 4:col){
    #按照列，替换每一列的NA值为该列的平均值(无法操作)
    fl[is.na(fl[,i]),i] <- mean(fl[,i],na.rm = TRUE)
  }
  #写出文件
  write.csv(fl, "F_combine.csv",row.names=FALSE,col.names=TRUE)
  return(fl)
}

preprocess_Reverse_f <- function(fl,items,counting) {
  for (t in 1:length(items)){
    #first change
    changes = 100
    que = counting
    for (i in 1:que){
      fl[,items[t]][which(fl[,items[t]]==i)] <- changes
      i = i + 1
      changes = changes + 1
    }
    #second change
    for (j in 100:(100+que-1)){
      fl[,items[t]][which(fl[,items[t]]==j)] <- que
      j = j + 1
      que = que - 1
    }
  }
  return(fl)
}

questionnaire_select_f <- function(fl,select,iFs){
  #确定问卷数量和计算次数
  num <- length(select)
  for (i in 1:num) {
    #反向计分,确定反向计分列和行数
    #合成各量表总分分数
    if (select[i] == 1){
      ##人际反应指针 IRI
      ##fl$IRI07,fl$IRI12,fl$IRI03,fl$IRI15,fl$IRI04,fl$IRI14,fl$IRI18,fl$IRI13,fl$IRI19fl
      iF = iFs[1]
      items = c(iF+6,iF+11,iF+2,iF+14,iF+3,iF+13,iF+17,iF+12,iF+18)
      counting = 5
      fl <- preprocess_Reverse_f(fl,items,counting)
      # 人际反应指针，IRI，28个条目，2个维度
      fl$IRI = apply(fl[iF:(iF+27)],1,mean)
      # COGEM 1,3*,5,7*,8,11,12*,15*,16,21,23,25,26,28
      fl$IRICog = apply(fl[c(iF,iF+2,iF+4,iF+6,iF+7,iF+10,iF+11,
                             iF+14,iF+15,iF+20,iF+22,iF+24,iF+25,iF+27)],1,mean)
      # AFFGEM 2,4*,6,9,10,13*,14*,17,18*,19*,20,22,24,27
      fl$IRIAff = apply(fl[c(iF+1,iF+3,iF+5,iF+8,iF+9,iF+12,iF+13,iF+16,
                             iF+17,iF+18,iF+19,iF+21,iF+23,iF+26)],1,mean)
      # IRIper
      fl$IRIper = apply(fl[c(iF+2,iF+7,iF+10,iF+14,iF+20,iF+24,iF+27)],1,mean)
      # IRIemp
      fl$IRIemp = apply(fl[c(iF+1,iF+3,iF+8,iF+13,iF+17,iF+19,iF+21)],1,mean)
      
    }
  }
  return(fl)
}

main_f <- function(){
  #删除当前所有对象
  rm(list=ls(all=TRUE))
  
  #更改操作路径
  route <- "C:/Users/yanzq/Desktop/"
  setwd(route)
  
  #替换空值和缺失值，用均值替代
  fl <- replace_empty_f(fl)
  
  #读取csv文件	
  fl <- read.csv("F_combine.csv",header=TRUE)
  #summary(fl)
  #names(fl) #获取完整的数据变量名
  #str(fl) #查看数据结构
  
  # 1 = IRI
  select = c(12)
  first_line = c(5)
  # 确认对应问卷的初始行,无数据行也需要用NA填充 #
  iFs = rep(NA,16) 
  # 进行自动替换，需要确认select即可
  for (i in 1:length(select)){
    iFs[select[i]] <- first_line[i]
  }
  # 进行问卷分数计算
  fl <- questionnaire_select_f(fl,select,iFs)
  
  #写出文件
  write.csv(fl, "final_combine.csv",row.names=FALSE,col.names=TRUE)
}

#主函数
main_f()