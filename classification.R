library(caret)
library(randomForest)
library(xgboost)
library(plyr)
library(cvTools)
library(lsa)
library(MASS)

data<-read.csv("dtms.csv")
#ctrl <- trainControl(method = "cv",number=5, savePredictions=TRUE)
#training<-data[,1:(ncol(data)-1)]
#journalName<-as.factor(data[,ncol(data)])
#data<-data[,1:(ncol(data)-1)]
#rfFit<- train(journalName ~ ., data=data, method = "xgbTree", trControl = ctrl)
#rfFit<-train(data,journalName,methof="xgbTree",trControl = ctrl)
#rfFit$pred
#output<-randomForest(data,journalName)

#Create Train and test set
nf<-10
accuracy<-c()
folds<-cvFolds(nrow(data),K=nf,R=1)
#folds<-cvFolds(ncol(data),K=nf,R=1)
for (i in 1:nf) {
  print("hi")
  #Create trainset
  trainLabel<-as.factor(data[folds$subsets[folds$which!=i],ncol(data)])
  trainset<-data[folds$subsets[folds$which!=i],1:(ncol(data)-1)]
  #trainset<-data[1:nrow(data),folds$subsets[folds$which!=i]]
  r<-lda(trainset,trainLabel)
  #trainset<-as.textmatrix(trainset)
  mat<-lsa(trainset,dims=100)
  tk<-as.data.frame(mat$tk)
  #rownames(tk)<-make.names(tk[1:nrow(tk),1],unique = TRUE) #relabel unique row names in dtms
  #tk<-cbind(tk,journaldata$journal) #add class label name
  #colnames(tk)[ncol(tk)]<-"journalName" #rename class column
  
  #Create testset
  testLabel<-as.factor(data[folds$subsets[folds$which==i],ncol(data)])
  #testset<-data[folds$subsets[folds$which==i],1:(ncol(data)-1)]
  testset<-data[1:nrow(data),folds$subsets[folds$which==i]]
  testset<-as.textmatrix(testset)
  tmat<-fold_in(testset,mat)
  #Random Forest
  output<-randomForest(trainset,trainLabel,testset,testLabel)
  #Store confusion matrix
  cm<-output$test$confusion
  sumCM<-0
  for (j in 1:nrow(cm)) {
    sumCM<-sumCM+cm[j,j]
  }
  accuracy[i]<-sumCM/sum(cm[,1:ncol(cm)-1])
  print(paste0("fold=",i, ",  accuracy=",accuracy[i]))
}
