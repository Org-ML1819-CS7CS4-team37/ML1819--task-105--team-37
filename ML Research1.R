library(xlsx)
library(dplyr)
library(caret)
install.packages('mice')
library(mice)

Read_Data<-function() 
{
  Training<-read.csv('C:/Users/yadavra/Downloads/aps_failure_training_set.csv',sep = ",",skip = 20,header = TRUE)
  Testing<-read.csv('C:/Users/yadavra/Downloads/aps_failure_test_set.csv',sep = ",",skip = 20,header = TRUE)
  
  head(Training,5)
  head(Testing,5)
  
  str(Training)
  Str(Testing)
  
  summary(Training)
  summary(Testing)
  
}

Data_Ananlysis<- function()
{
  is.na(Training) <- Training == "na"
  is.na(Testing)<-Testing=="na"
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
  
  Training_data<-Training
  Testing_data<-Testing
  Training_data$set<- "Train"
  Testing_data$set<-"Test"
  
  Final_Dataset<-rbind(Training_data,Testing_data)
  
}

md.pattern(Training)
library("VIM")
mice_plot <- aggr(Training, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(Training), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
mice_plot_test <- aggr(Testing, col=c('navyblue','yellow'),
                       numbers=TRUE, sortVars=TRUE,
                       labels=names(Training), cex.axis=.7,
                       gap=3, ylab=c("Missing data","Pattern"))

##is.na(Training) <- Training == "na"
Data_Cleaning <- function()
{
  
  i=ncol(Final_Dataset)
  output <- matrix(ncol=2, nrow=i)
  for (i in 2:i)
  { 
    print(i)
    print(((length(which(is.na(Final_Dataset[,i]))))/length(Final_Dataset[,i]))*100)
    output[i,1]<-i
    output[i,2]<-((length(which(is.na(Final_Dataset[,i]))))/length(Final_Dataset[,i]))*100
    
  }
  #### > 75 % of missing values
  (which(output[,2]>75))
  Dataclean_Training<- Final_Dataset[,-(which(output[,2]>75))]
  dim(Dataclean_Training)
  i=ncol(Dataclean_Training)
  output1 <- matrix(ncol=2, nrow=i)
  for (i in 2:i)
  { 
    print(i)
    print(length(which(Dataclean_Training[,i]==0))/nrow(Dataclean_Training)*100)
    output1[i,1]<-i
    output1[i,2]<-length(which(Dataclean_Training[,i]==0))/nrow(Dataclean_Training)*100
    
  }
  #### remove with more zero
  (which(output1[,2]>75))
  Dataclean_Training<- Dataclean_Training[,-(which(output1[,2]>75))]
  dim(Dataclean_Training)
}

compute_Missing <- function()
{
  Training_data<-Dataclean_Training
  Testing_data<-Testing
  Training_data$set<- "Train"
  Testing_data$set<-"Test"
  
  Final_Dataset<-rbind(Training_data,Testing_data)
  dim(Final_Dataset)
  set.seed(123)
  imputed_full <- mice(Dataclean_Training[,-c(1,126)], 
                       m=1, 
                       maxit = 2, 
                       method = "mean", 
                       seed = 500)
}


#test=Dataclean_Training
dim(Dataclean_Training)
for(i in 3:125)
{
  Dataclean_Training[,i]<-as.numeric(Dataclean_Training[,i])
}
dim(Dataclean_Training)
str(Dataclean_Training)

source("https://bioconductor.org/biocLite.R")
biocLite("pcaMethods")
library(pcaMethods)
tra_pca <- pca(Dataclean_Training, method="bpca")
tra_mi <- as.data.frame(cbind(Dataclean_Training$class, completeObs(tra_pca)))
