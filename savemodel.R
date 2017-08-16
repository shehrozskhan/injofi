#This script trains the best classifier and save the model 

rm(list=ls()) #clear workspace

library(xgboost)
library(tm)
library(plyr)
library(dplyr)

#journaldata<-read.csv(file="26aijournals.csv") #read the input data file
journaldata1<-read.csv(file="26aijournals.csv") #read the input data file
journaldata2<-read.csv(file="44infsysjournals.csv")

#remove the common papers from both files
common<-intersect(journaldata1,journaldata2)
x<-journaldata2$journal %in% common$journal
indexCommon<-as.numeric(rownames(journaldata2[x,]))
journaldata2<-journaldata2[-indexCommon,]

journaldata<-rbind(journaldata1,journaldata2) #Combine

#Frequency of journals' papers
papers<-plyr::count(journaldata,'journal')
#If number of papers are < 100, ignore that journal - to remove insignificant journals
papers<-papers[papers$freq>100,]
weights<-max(papers$freq)/papers$freq #Weigh the frequency of papers invesely
papers<-cbind(papers,weights) #Add the weights column
journaldata<-join(journaldata,papers,by = "journal",type="inner")
weights<-journaldata$weights #extract weights
journaldata<-dplyr::select(journaldata,c(-freq,-weights)) #remove column 'freq' and 'weights'
journaldata$journal<-factor(journaldata$journal) #change the levels to accomodate removed (papers from) journals

print('Reading text from scraped data')
source("readTextData.R")

#Create Corpus
print('Creating Training Corpus')
source("createCorpus.R")
corpus<-getcorpus(textdata)
label<-label-1

#Create document-term frequency
print('Creating Document Term Frequency')
source("createdtm.R")
dtms<-getdtm(corpus)
train.col <- colnames(dtms[,1:ncol(dtms)]) # training

#Train XGBoost 
print('Training XGBoost')
xg<-xgboost(as.matrix(dtms),label,nrounds=50,objective="multi:softprob",num_class=length(unique(label)))

xgb.save(xg,'injofi.model')
save(train.col,uniqueCL,file='param')

#xgmodel<-xgb.load('xgboost.10ai.model')
