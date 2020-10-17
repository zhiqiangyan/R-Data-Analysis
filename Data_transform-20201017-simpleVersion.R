#�����д����Ϊ20200404
#by zhiqiang yan

replace_empty_f <- function(fl){
  #��ȡcsv�ļ�	
  fl <- read.csv("10.12_data.csv",header=TRUE,sep=",",na.strings = "999")
  #��ȡ�к���
  row <- dim(fl)[1]  
  col <- dim(fl)[2]
  
  for (i in 4:col){
    #�����У��滻ÿһ�е�NAֵΪ���е�ƽ��ֵ(�޷�����)
    fl[is.na(fl[,i]),i] <- mean(fl[,i],na.rm = TRUE)
  }
  #д���ļ�
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
  #ȷ���ʾ������ͼ������
  num <- length(select)
  for (i in 1:num) {
    #����Ʒ�,ȷ������Ʒ��к�����
    #�ϳɸ������ַܷ���
    if (select[i] == 1){
      ##�˼ʷ�Ӧָ�� IRI
      ##fl$IRI07,fl$IRI12,fl$IRI03,fl$IRI15,fl$IRI04,fl$IRI14,fl$IRI18,fl$IRI13,fl$IRI19fl
      iF = iFs[1]
      items = c(iF+6,iF+11,iF+2,iF+14,iF+3,iF+13,iF+17,iF+12,iF+18)
      counting = 5
      fl <- preprocess_Reverse_f(fl,items,counting)
      # �˼ʷ�Ӧָ�룬IRI��28����Ŀ��2��ά��
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
  #ɾ����ǰ���ж���
  rm(list=ls(all=TRUE))
  
  #���Ĳ���·��
  route <- "C:/Users/yanzq/Desktop/"
  setwd(route)
  
  #�滻��ֵ��ȱʧֵ���þ�ֵ���
  fl <- replace_empty_f(fl)
  
  #��ȡcsv�ļ�	
  fl <- read.csv("F_combine.csv",header=TRUE)
  #summary(fl)
  #names(fl) #��ȡ���������ݱ�����
  #str(fl) #�鿴���ݽṹ
  
  # 1 = IRI
  select = c(12)
  first_line = c(5)
  # ȷ�϶�Ӧ�ʾ��ĳ�ʼ��,��������Ҳ��Ҫ��NA��� #
  iFs = rep(NA,16) 
  # �����Զ��滻����Ҫȷ��select����
  for (i in 1:length(select)){
    iFs[select[i]] <- first_line[i]
  }
  # �����ʾ���������
  fl <- questionnaire_select_f(fl,select,iFs)
  
  #д���ļ�
  write.csv(fl, "final_combine.csv",row.names=FALSE,col.names=TRUE)
}

#������
main_f()