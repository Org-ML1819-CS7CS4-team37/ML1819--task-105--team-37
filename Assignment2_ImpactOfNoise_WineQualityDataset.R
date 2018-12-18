######################## Data Load#################

Data_Census <- read.csv("C:/Users/Rajath/Downloads/winequality-white.csv",header = TRUE,sep = ";")

head(Data_Census)
cor(Data_Census)
hist(Data_Census$quality)
hist(Data_Census$quality, 
     main="Histogram for Air Passengers", 
     xlab="Passengers", 
     border="blue", 
     col="green", 
    
     las=1, 
     breaks=5, 
     prob = TRUE)
install.packages("ggplot2")
library(ggplot2)

qplot(Data_Census$quality,
      geom="histogram",
      binwidth = 0.50,  
      main = "Histogram for Quality", 
      xlab = "Quality", 
      fill=I("Blue"), 
      col=I("red"), 
      alpha=I(.6))

############################## Data Preprocessinbg ###################
install.packages("corrplot")
library(corrplot)
corrplot(cor(Data_Census), method="circle")


boxplot(Data_Census$quality,Data_Census)
############################ Preprocessing

normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}

hist(Data_Census$fixed.acidity)
hist(Data_Census$volatile.acidity)
hist(Data_Census$citric.acid)
hist(Data_Census$residual.sugar)
Data_Census$alcohol<-normalize(Data_Census$alcohol)

head(Data_Census)

############## Feature Engineering ################
install.packages("caret")
library(caret)
class(Data_Census$quality)

Data_Census$quality[which(Data_Census$quality<=5)]=0
Data_Census$quality[which(Data_Census$quality==6)]=1
Data_Census$quality[which(Data_Census$quality==7)]=1
Data_Census$quality[which(Data_Census$quality>=8)]=2
Data_Census$quality<-as.factor(Data_Census$quality)

dpart <- createDataPartition(Data_Census$quality, p = 0.7, list = F)

Data_Train<-Data_Census[dpart,]
Data_Test<-Data_Census[-dpart,]
Data_Train$quality


train_control<- trainControl(method="cv", number=10)
tunegrid <- expand.grid(.mtry=6)
RF_Feature_Engineering <- train(quality~. , 
                                data=Data_Train,
                                method = "rf",trControl = train_control,tuneGrid=tunegrid)





control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(quality~., data=Data_Train, method="rf", metric="Accuracy", tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)



#,tuneGrid=tunegrid
#tuneGrid = data.frame(fL=0, usekernel=FALSE,adjust=FALSE)
predrf <- predict(RF_Feature_Engineering, Data_Test)
colnames(Data_Train)
t2<-table(Data_Test$quality,predrf)
confusionMatrix(Data_Test$quality,predrf)

VarIm<-varImp(RF_Feature_Engineering)
plot(varImp(RF_Feature_Engineering))
colnames(varImp(RF_Feature_Engineering))


train_control<- trainControl(method="cv", number=5)
NB_Feature_Engineering <- train(quality~. , 
                                data=Data_Train,
                                method = "nb",trControl = train_control,tuneGrid = data.frame(fL=0, usekernel=TRUE,adjust=1))
#            trControl = trainControl(method="none"),
#tuneGrid = data.frame(fL=0, usekernel=TRUE,adjust=3)
predNB <- predict(NB_Feature_Engineering, Data_Test)
Data_Test$quality<-as.factor(Data_Test$quality)

t<-table(Data_Test$quality,predNB)
confusionMatrix(Data_Test$quality,predNB)









##################### NOISE (irrelavent Feature)
Noise <- read.table("C:/Raksha/Scalable/AirQualityUCI/AirQualityUCI.csv",header = TRUE,sep=",")
NROW(Data_Census)
NROW(Noise)
Noise_final<-Noise[1:4898,]
NROW(Noise_final)
Data_noiose<-Data_Census
Data_noiose<-cbind(Data_noiose,Noise_final[,1:9])
NROW(Data_noiose)

Data_noiose$quality[which(Data_noiose$quality<=5)]=0
Data_noiose$quality[which(Data_noiose$quality==6)]=1
Data_noiose$quality[which(Data_noiose$quality==7)]=1
Data_noiose$quality[which(Data_noiose$quality>=8)]=2
Data_noiose$quality<-as.factor(Data_noiose$quality)

dpart <- createDataPartition(Data_noiose$quality, p = 0.7, list = F)

Data_Train<-Data_noiose[dpart,]
Data_Test<-Data_noiose[-dpart,]
Data_noiose$quality
head(Data_Train)


train_control<- trainControl(method="cv", number=10)
tunegrid <- expand.grid(.mtry=6)
RF_Feature_Engineering <- train(quality~. , 
                                data=Data_Train,
                                method = "rf",trControl = train_control,tuneGrid=tunegrid)
#,tuneGrid=tunegrid
#tuneGrid = data.frame(fL=0, usekernel=FALSE,adjust=FALSE)
predrf <- predict(RF_Feature_Engineering, Data_Test)
colnames(Data_Train)
t2<-table(Data_Test$quality,predrf)
confusionMatrix(Data_Test$quality,predrf)

VarIm<-varImp(RF_Feature_Engineering)
plot(varImp(RF_Feature_Engineering))
colnames(varImp(RF_Feature_Engineering))


train_control<- trainControl(method="cv", number=5)
NB_Feature_Engineering <- train(quality~. , 
                                data=Data_Train,
                                method = "nb",trControl = train_control)
#            trControl = trainControl(method="none"),
#tuneGrid = data.frame(fL=0, usekernel=TRUE,adjust=3)
predNB <- predict(NB_Feature_Engineering, Data_Test)
Data_Test$quality<-as.factor(Data_Test$quality)

t<-table(Data_Test$quality,predNB)
confusionMatrix(Data_Test$quality,predNB)



############################## Gaussian Noise  10 % , 25 % 40 %

Noise <- read.table("C:/Raksha/Scalable/AirQualityUCI/AirQualityUCI.csv",header = TRUE,sep=",")
NROW(Data_Census)
dim(Noise)
head(Noise)
Noise_final<-(Noise[1:2000,2:13])
colnames(Noise_final)=colnames(Data_noiose)
Noise_final[12]
Data_noiose<-Data_Census

install.packages("gtools")
library(gtools)
class(Noise_final$quality)
NROW(Data_noiose)
Noise_final$quality[which(Noise_final$quality<=12.9)]=0
Noise_final$quality[which(Noise_final$quality>=13 | Noise_final$quality<=14.9)]=1

Noise_final$quality[which(Noise_final$quality>=15)]=2
Data_noiose$quality[which(Data_noiose$quality<=5)]=0
Data_noiose$quality[which(Data_noiose$quality==6)]=1
Data_noiose$quality[which(Data_noiose$quality==7)]=1
Data_noiose$quality[which(Data_noiose$quality>=8)]=2
Noise_final$quality<-as.factor(Noise_final$quality)
Noise_final$quality
dim(Noise_final)
Data_noiose<-rbind(Data_noiose[1:2000,],Noise_final)
dim(Data_noiose)
dim(Noise_final)
dpart <- createDataPartition(Data_noiose$quality, p = 0.7, list = F)

Data_Train<-Data_noiose[dpart,]
Data_Test<-Data_noiose[-dpart,]
Data_Test$quality
head(Data_Train)


train_control<- trainControl(method="cv", number=10)
tunegrid <- expand.grid(.mtry=6)
RF_Feature_Engineering <- train(quality~. , 
                                data=Data_Train,
                                method = "rf",trControl = train_control,tuneGrid=tunegrid)
#,tuneGrid=tunegrid
#tuneGrid = data.frame(fL=0, usekernel=FALSE,adjust=FALSE)
predrf <- predict(RF_Feature_Engineering, Data_Test)
colnames(Data_Train)
t2<-table(Data_Test$quality,predrf)
confusionMatrix(Data_Test$quality,predrf)

VarIm<-varImp(RF_Feature_Engineering)
plot(varImp(RF_Feature_Engineering))
colnames(varImp(RF_Feature_Engineering))


train_control<- trainControl(method="cv", number=5)
NB_Feature_Engineering <- train(quality~. , 
                                data=Data_Train,
                                method = "nb",trControl = train_control)
#            trControl = trainControl(method="none"),
#tuneGrid = data.frame(fL=0, usekernel=TRUE,adjust=3)
predNB <- predict(NB_Feature_Engineering, Data_Test)
Data_Test$quality<-as.factor(Data_Test$quality)

t<-table(Data_Test$quality,predNB)
confusionMatrix(Data_Test$quality,predNB)











################################## feature replace 10% 25% 40%

Noise <- read.table("C:/Raksha/Scalable/AirQualityUCI/AirQualityUCI.csv",header = TRUE,sep=",")
NROW(Data_Census)
dim(Noise)
head(Noise)
Noise_final<-(Noise[1:4898,3:4])

Noise_final[12]
Data_noiose<-Data_Census

install.packages("gtools")
library(gtools)
class(Noise_final$quality)
NROW(Data_noiose)

dim(Noise_final)
Data_noiose<-cbind(Data_noiose[,3:12],Noise_final)
dim(Data_noiose)
dim(Noise_final)
dpart <- createDataPartition(Data_noiose$quality, p = 0.7, list = F)

Data_Train<-Data_noiose[dpart,]
Data_Test<-Data_noiose[-dpart,]
Data_Test$quality
head(Data_Train)


train_control<- trainControl(method="cv", number=10)
tunegrid <- expand.grid(.mtry=6)
RF_Feature_Engineering <- train(quality~. , 
                                data=Data_Train,
                                method = "rf",trControl = train_control,tuneGrid=tunegrid)
#,tuneGrid=tunegrid
#tuneGrid = data.frame(fL=0, usekernel=FALSE,adjust=FALSE)
predrf <- predict(RF_Feature_Engineering, Data_Test)
colnames(Data_Train)
t2<-table(Data_Test$quality,predrf)
confusionMatrix(Data_Test$quality,predrf)

VarIm<-varImp(RF_Feature_Engineering)
plot(varImp(RF_Feature_Engineering))
colnames(varImp(RF_Feature_Engineering))


train_control<- trainControl(method="cv", number=5)
NB_Feature_Engineering <- train(quality~. , 
                                data=Data_Train,
                                method = "nb",trControl = train_control)
#            trControl = trainControl(method="none"),
#tuneGrid = data.frame(fL=0, usekernel=TRUE,adjust=3)
predNB <- predict(NB_Feature_Engineering, Data_Test)
Data_Test$quality<-as.factor(Data_Test$quality)

t<-table(Data_Test$quality,predNB)
confusionMatrix(Data_Test$quality,predNB)

