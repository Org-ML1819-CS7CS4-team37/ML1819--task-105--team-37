library(xlsx)
library(dplR)
install.packages("dplR") 
library(caret)
install.packages('mice')
library(mice)

Read_Data<-function() 
{
## Read Data  
  Training<-read.csv('C:/Users/Rajath/Downloads/aps_failure_training_set.csv',sep = ",",skip = 20,header = TRUE)
  Testing<-read.csv('C:/Users/Rajath/Downloads/aps_failure_test_set.csv',sep = ",",skip = 20,header = TRUE)
  
  head(Training,5)
  head(Testing,5)
## View Data  
  str(Training)
  Str(Testing)
  
  summary(Training)
  summary(Testing)
  
}

Data_Ananlysis<- function()
{
# Data formatting issue , NA are recognised as character "NA"  
  is.na(Training) <- Training == "na"
  is.na(Testing)<-Testing=="na"
  (length(which(is.na(Training[,3]))))
  (length(which(!is.na(Training[,3]))))
  
  (length(which(is.na(Testing[,3]))))
  (length(which(!is.na(Testing[,3]))))
##  Look percentage of NA in each feature
  i=ncol(Training)
  output <- matrix(ncol=2, nrow=i)
  for (i in 2:i)
  { 
    print(i)
    print((length(which(is.na(Training[,i]))))/length(Training[,i])*100)
    output[i,1]<-i
    output[i,2]<-(length(which(is.na(Training[,i]))))/length(Training[,i])*100
    
  }
 
### add new coloumn as Train and Test for 
  Training_data<-Training
  Testing_data<-Testing
  Training_data$set<- "Train"
  Testing_data$set<-"Test"
  
### concatenate both data set to clean and impute missing values  
  
  Final_Dataset<-rbind(Training_data,Testing_data)
  dim(Final_Dataset)
  
  
  
}



Data_Cleaning <- function()
{
### get the percentage of Missig values  
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
#### remove with more than 75 % zero
  (which(output1[,2]>75))
  Dataclean_Training<- Dataclean_Training[,-(which(output1[,2]>75))]
  dim(Dataclean_Training)
}

compute_Missing <- function()
{
### look at the pattren of misisng data after clenaing most less coloumn data
  install.packages("mice")
  library(mice)
  md.pattern(Dataclean_Training)
  library("VIM")
  ### only for navie bayse
  ###Dataclean_Training<-Final_Dataset
  mice_plot <- aggr(Dataclean_Training, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(Dataclean_Training), cex.axis=0.7,
                    gap=5,main="Features",ylab=c("Missing data percentage","Pattern of Missing Data"),
                    )

  dim(Dataclean_Training)
  str(Dataclean_Training)
  i=ncol(Dataclean_Training)
## exclude the coloumns of format category 
  for(i in 2:i-1)
  {
    Dataclean_Training[,i]<-as.numeric(Dataclean_Training[,i])
  }
  dim(Dataclean_Training)
  str(Dataclean_Training)
  
  set.seed(123)
## Impute Missing data  
  Data_imputed <- mice(Dataclean_Training[,-c(1,ncol(Dataclean_Training))], 
                       m=1, 
                       maxit = 2, 
                       method = "mean", 
                       seed = 500)
  
## get the one set of imputed data set
  FinalDataImpute<-complete(Data_imputed, 1)

## look for the pattren again after imputation 
  md.pattern(FinalDataImpute)
 
   mice_plot <- aggr(FinalDataImpute, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(FinalDataImpute), cex.axis=0.7,
                    gap=3,xlab=c("x","y"), ylab=c("Missing data percentage","Pattern of Missing Data"))
  str(FinalDataImpute)

  
# look for loogged events if any collinera coloumns recognised my mice and remove  
  ColwithstillNa <- (Data_imputed$loggedEvents[-1,])
  print(ColwithstillNa[,"out"])
  FinalDataImpute<-FinalDataImpute[,-c(ColwithstillNa[,"out"])]
  dim(FinalDataImpute)
  FinalDataImpute<-cbind(Dataclean_Training[,c(1,ncol(Dataclean_Training))],FinalDataImpute)
  training_data_fin <- subset(FinalDataImpute, set == "Train")
  test_data_fin <- subset(FinalDataImpute, set == "Test")
  dim(training_data_fin)
  dim(test_data_fin)
  training_data_fin$set<-NULL
  test_data_fin$set<-NULL
  training_data_fin$class<-as.integer(training_data_fin$class)
  test_data_fin$class<-as.integer(test_data_fin$class)
 
  
  install.packages("corrplot")
  library(corrplot)
  corrplot::corrplot(cor(training_data_fin),method = c("color"))
}

modelfit<- function()
{
  install.packages("caret")
  library(caret)
  install.packages("klaR")
  library(klaR)
  
### cross verify if any coloumn still with NA  
  i=ncol(training_data_fin)
  output <- matrix(ncol=2, nrow=i)
  for (i in 2:i)
  { 
    print(i)
    print((length(which(is.na(training_data_fin[,i]))))/length(training_data_fin[,i])*100)
    output[i,1]<-i
    output[i,2]<-(length(which(is.na(training_data_fin[,i]))))/length(training_data_fin[,i])*100
    
  }
  training_data_fin<- training_data_fin[,-output[which(output[,2]>0)]]
  dim(training_data_fin)
  test_data_fin<-test_data_fin[,-output[which(output[,2]>0)]]
  dim(test_data_fin)
  
  corrplot::corrplot(cor(training_data_fin),method = c("color"))
  training_data_fin$class<-as.factor(training_data_fin$class)
  test_data_fin$class<-as.factor(test_data_fin$class)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  library(caret)
  train_control <- trainControl(method="cv", number=2)
  # Fit Naive Bayes Model
  model <- train(class~., data=training_data_fin, trControl=train_control, method="nb")
  # Summarise Results
  print(model)
  t_kfold<-predict(model,test_data_fin)
  NBinfkfold<-confusionMatrix(t_kfold,test_data_fin$class,positive = "1")
  
  
  
  

  
### NB Modle fit  
  modle<-NaiveBayes(training_data_fin,factor(training_data_fin$class))
  t<-predict(modle,test_data_fin)
  NBinf<-confusionMatrix(t$class,test_data_fin$class,positive = "1")
  #nb_model <- NaiveBayes(as.logical(class) ~ ., data = training_data_fin)
  # Confusion Matrix and Statistics
  # 
  # Reference
  # Prediction   neg   pos
  # neg 14585     5
  # pos  1040   370
  # 
  # Accuracy : 0.9347          
  # 95% CI : (0.9307, 0.9385)
  # No Information Rate : 0.9766          
  # P-Value [Acc > NIR] : 1               
  # 
  # Kappa : 0.3921          
  # Mcnemar's Test P-Value : <2e-16          
  #                                         
  #           Sensitivity : 0.98667         
  #           Specificity : 0.93344         
  #        Pos Pred Value : 0.26241         
  #        Neg Pred Value : 0.99966         
  #            Prevalence : 0.02344         
  #        Detection Rate : 0.02312         
  #  Detection Prevalence : 0.08812         
  #     Balanced Accuracy : 0.96005         
  #                                         
  #      'Positive' Class : pos


  
### random Forest Modle Fit  
  Rfit<-randomForest::randomForest(factor(class) ~.,data=training_data_fin) ## find importance
       M_VImp<-Rfit$importance
  write.table(utils::View(M_VImp),file = "C:/Users/Rajath/Downloads/steps.xlsx")
 


  
  # Confusion Matrix and Statistics
  # 
  # Reference
  # Prediction     1     2
  # 1 15548   232
  # 2    77   143
  # 
  # Accuracy : 0.9807          
  # 95% CI : (0.9784, 0.9828)
  # No Information Rate : 0.9766          
  # P-Value [Acc > NIR] : 0.0002163       
  # 
  # Kappa : 0.4715          
  # Mcnemar's Test P-Value : < 2.2e-16       
  # 
  # Sensitivity : 0.9951          
  # Specificity : 0.3813          
  # Pos Pred Value : 0.9853          
  # Neg Pred Value : 0.6500          
  # Prevalence : 0.9766          
  # Detection Rate : 0.9718          
  # Detection Prevalence : 0.9862          
  # Balanced Accuracy : 0.6882          
  # 
  # 'Positive' Class : 1      
  
  randomForest::varImpPlot(Rfit,main = "Variable importance plot using Random Forest")
  predicR<-predict(Rfit,test_data_fin)
  RfitCM<-confusionMatrix(predicR,test_data_fin$class)
  # Confusion Matrix and Statistics
  # 
  # Reference
  # Prediction   neg   pos
  # neg 14840    60
  # pos   785   315
  # 
  # Accuracy : 0.9472          
  # 95% CI : (0.9436, 0.9506)
  # No Information Rate : 0.9766          
  # P-Value [Acc > NIR] : 1               
  # 
  # Kappa : 0.4064          
  # Mcnemar's Test P-Value : <2e-16          
  #                                         
  #           Sensitivity : 0.84000         
  #           Specificity : 0.94976         
  #        Pos Pred Value : 0.28636         
  #        Neg Pred Value : 0.99597         
  #            Prevalence : 0.02344         
  #        Detection Rate : 0.01969         
  #  Detection Prevalence : 0.06875         
  #     Balanced Accuracy : 0.89488         
  #                                         
  #      'Positive' Class : pos    
  
  
  
### Feature having weightage greater than 25  
  finfea<-which(M_VImp>20) # Important var greater than 25
  newimpdatatrain<-training_data_fin[,c(finfea,1)]
  newimpdatatest<-test_data_fin[,c(finfea,1)]
  dim(newimpdatatest)
  
  
  
  
  
  
  train_control <- trainControl(method="cv", number=2)
  # Fit Naive Bayes Model
  model1 <- train(class~., data=newimpdatatrain, trControl=train_control, method="nb")
  # Summarise Results
  print(model)
  t_kfold1<-predict(model1,newimpdatatest)
  NBinfkfold1<-confusionMatrix(t_kfold1,newimpdatatest$class,positive = "1")
  
  
  
  modelrf <- train(class~., data=training_data_fin, trControl=train_control, method="rf")
  # Summarise Results
  print(model)
  rf_kfold<-predict(modelrf,test_data_fin)
  rfkfold<-confusionMatrix(rf_kfold,test_data_fin$class,positive = "1")
  
  
  
  
  modelrf1 <- train(class~., data=newimpdatatrain, trControl=train_control, method="rf")
  # Summarise Results
  print(model)
  rf_kfold1<-predict(modelrf1,newimpdatatest)
  rfkfold1<-confusionMatrix(rf_kfold1,newimpdatatest$class,positive = "1")
  
  
# ### RF fit after fetaure selection  
#   Rfittrail1<-randomForest::randomForest(factor(class) ~.,data=newimpdatatrain) ## find importance
#   impdatatrail1<-randomForest::varImpPlot(Rfit)
#   predicRtrail1<-predict(Rfittrail1,newimpdatatest)
#   RfitCMtrail1<-confusionMatrix(predicRtrail1,newimpdatatest$class)
#   
#   # > RfitCMtrail1
#   # Confusion Matrix and Statistics
#   # 
#   # Reference
#   # Prediction   neg   pos
#   # neg 15615   324
#   # pos    10    51
#   # 
#   # Accuracy : 0.9791          
#   # 95% CI : (0.9768, 0.9813)
#   # No Information Rate : 0.9766          
#   # P-Value [Acc > NIR] : 0.0159          
#   # 
#   # Kappa : 0.2289          
#   # Mcnemar's Test P-Value : <2e-16          
#   #                                         
#   #           Sensitivity : 0.136000        
#   #           Specificity : 0.999360        
#   #        Pos Pred Value : 0.836066        
#   #        Neg Pred Value : 0.979673        
#   #            Prevalence : 0.023438        
#   #        Detection Rate : 0.003187        
#   #  Detection Prevalence : 0.003812        
#   #     Balanced Accuracy : 0.567680        
#   #                                         
#   #      'Positive' Class : pos             
#   
#   
#   
# # NB fit after feature selection  
#   modletrail1<-NaiveBayes(newimpdatatrain,as.factor( newimpdatatrain$class))
#   ttrail1<-predict(modletrail1, newimpdatatest)
#   NBinftrail1<-confusionMatrix(ttrail1$class,newimpdatatest$class)
#   # NBinftrail1
#   # Confusion Matrix and Statistics
#   # 
#   # Reference
#   # Prediction   neg   pos
#   # neg 15118    17
#   # pos   507   358
#   # 
#   # Accuracy : 0.9672        
#   # 95% CI : (0.9644, 0.97)
#   # No Information Rate : 0.9766        
#   # P-Value [Acc > NIR] : 1             
#   # 
#   # Kappa : 0.5631        
#   # Mcnemar's Test P-Value : <2e-16        
#   #                                       
#   #           Sensitivity : 0.95467       
#   #           Specificity : 0.96755       
#   #        Pos Pred Value : 0.41387       
#   #        Neg Pred Value : 0.99888       
#   #            Prevalence : 0.02344       
#   #        Detection Rate : 0.02237       
#   #  Detection Prevalence : 0.05406       
#   #     Balanced Accuracy : 0.96111       
#   #                                       
#   #      'Positive' Class : pos  
# 
# ### looking for more feature filter but values are not much changing    
#   finfea<-which(M_VImp>13) # Important var greater than 13
#   newimpdatatrain<-training_data_fin[,finfea]
#   newimpdatatest<-test_data_fin[,finfea]
#   dim(newimpdatatest)
#   
#   Rfittrail2<-randomForest::randomForest(factor(class) ~.,data=newimpdatatrain) ## find importance
#   impdatatrail2<-randomForest::varImpPlot(Rfit)
#   predicRtrial2<-predict(Rfit,newimpdatatest)
#   RfitCMtrail2<-confusionMatrix(predicR,newimpdatatest$class,positive = "pos")
#   modletrail2<-NaiveBayes(newimpdatatrain,as.factor( newimpdatatrain$class))
#   ttrail2<-predict(modle, newimpdatatest)
#   NBinftrail2<-confusionMatrix(t$class,newimpdatatest$class,positive = "pos")
}

#### Look for outliers
outlier<- function()
  {
    
    cooksdata<-cooks.distance(glm(class ~ ., family = "binomial", data = training_data_fin))
    plot(cooksdata, 
            pch=".", 
          cex=2, 
         main="outliers")  
    outliers <- rownames(training_data_fin[cooksdata > 4*mean(cooksdata, na.rm=T), ])
    print(outliers)
    length(outliers)
    test<-training_data_fin[-c(outliers),]
    #abline(lm(class ~ ., data=cooksdata), col="yellow", lwd=3, lty=2)
    abline(h = 4*mean(cooksdata, na.rm=T), col="red")
    install.packages("EnvStats")
    library(EnvStats)
    dim(training_data_fin)
    rosnerTest(training_data_fin[,4],k=4)
    str(training_data_fin[,-c(1,2,120)])
  }

# 
# #*********************************** without clenainfg***********************
# 
# 
# 
# install.packages("mice")
# library(mice)
# md.pattern(Final_Dataset)
# library("VIM")
# mice_plot <- aggr(Final_Dataset, col=c('navyblue','yellow'),
#                   numbers=TRUE, sortVars=TRUE,
#                   labels=names(Training), cex.axis=.7,
#                   gap=3, ylab=c("Missing data","Pattern"))
# # mice_plot_test <- aggr(Testing, col=c('navyblue','yellow'),
# #                        numbers=TRUE, sortVars=TRUE,
# #                        labels=names(Training), cex.axis=.7,
# #                        gap=3, ylab=c("Missing data","Pattern"))
# 
# dim(Final_Dataset)
# str(Final_Dataset)
# i<-nclo(Final_Dataset)
# for(i in 3:i-1)
# {
#   Final_Dataset[,i]<-as.integer(Final_Dataset[,i])
# }
# dim(Final_Dataset)
# str(Final_Dataset)
# set.seed(123)
# Data_imputed <- mice(Final_Dataset[,-c(1,172,4)], 
#                      m=1, 
#                      maxit = 2, 
#                      method = "mean", 
#                      seed = 500)
# 
# FinalDataImpute<-complete(Data_imputed, 1)
# 
# md.pattern(FinalDataImpute)
# mice_plot <- aggr(FinalDataImpute, col=c('navyblue','yellow'),
#                   numbers=TRUE, sortVars=TRUE,
#                   labels=names(Training), cex.axis=.7,
#                   gap=3, ylab=c("Missing data","Pattern"))
# dim(FinalDataImpute)
# 
# ColwithstillNa <- (Data_imputed$loggedEvents)
# print(ColwithstillNa[,"out"])
# FinalDataImpute<-full_imputed[,-c(ColwithstillNa[,"out"])]
# dim(FinalDataImpute)
# FinalDataImpute<-cbind(Dataclean_Training[,c(1,172)],FinalDataImpute)
# training_data_fin <- subset(FinalDataImpute, set == "Train")
# test_data_fin <- subset(FinalDataImpute, set == "Test")
# dim(training_data_fin)
# dim(test_data_fin)
# training_data_fin$set<-NULL
# test_data_fin$set<-NULL
# 
# 
# i=ncol(training_data_fin)
# output <- matrix(ncol=2, nrow=i)
# for (i in 2:i)
# { 
#   print(i)
#   print((length(which(is.na(training_data_fin[,i]))))/length(training_data_fin[,i])*100)
#   output[i,1]<-i
#   output[i,2]<-(length(which(is.na(training_data_fin[,i]))))/length(training_data_fin[,i])*100
#   
# }
# 
# 
# md.pattern(training_data_fin)
# mice_plot <- aggr(training_data_fin, col=c('blue','yellow'),
#                   numbers=TRUE, sortVars=TRUE,
#                   labels=names(Training), cex.axis=.7,
#                   gap=3, ylab=c("Missing data","Pattern"))
# dim(FinalDataImpute)


##Plotting for comparison

NB1_1<-c(14585,1040)
NB1_2<c(5,370)
NB1<-c(0.9347,0.9866,0.9334)

NB2_1<-c(15615,10)
NB2_2<-c(324,51)
NB2<-c(0.9791,0.13600,0.9999)

RF1_1<-c(14840,785)
RF1_2<-c(60,315)
RF1<-c(0.9472,0.8400,0.9497)

RF2_1<-c(15118,507)
RF2_2<-c(17,358)
RF2<-c(0.9672,0.9546,0.9675)

rowname<-c("Accuracy","Specificity","Sensitivity")
Final_result<-rbind(cbind(rowname,NB1),cbind(rowname,RF1))
install.packages("ggplot2")
library(ggplot2)
# Basic line plot with points
ggplot(data=tes) +
  geom_line()

