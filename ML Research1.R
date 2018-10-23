library(xlsx)
library(dplyr)
library(caret)


Read_Data<-function() 
{
  Training<-read.csv('C:/Users/Rajath/Downloads/aps_failure_training_set.csv',sep = ",",skip = 20,header = TRUE)
  Testing<-read.csv('C:/Users/Rajath/Downloads/aps_failure_training_set.csv',sep = ",",skip = 20,header = TRUE)

  head(Training,5)
  head(Testing,5)
  
  str(Training)
  Str(Testing)
  
  summary(Training)
  summary(Testing)
  
}

Data_Ananlysis<- function()
{
  (length(which(is.na(Training[,3]))))
  (length(which(!is.na(Training[,3]))))
  
  (length(which(is.na(Testing[,3]))))
  (length(which(!is.na(Testing[,3]))))
  
  i=ncol(Training)
  output <- matrix(ncol=2, nrow=i)
  for (i in 2:i)
  { 
    print(i)
    print((length(which(is.na(Training[,i]))))/length(Training[,i])*100)
    output[i,1]<-i
    output[i,2]<-(length(which(is.na(Training[,i]))))/length(Training[,i])*100
  }
  
  
}


