#This script divides data into train and test set, find features for test set based on training and performs testing on unseen samples
#TODO -> should we remove numbers?

rm(list=ls()) #clear workspace

library(dplyr)
library(tm)
#library(qdap)
#library(SnowballC)
library(lsa)
library(cvTools)
library(MASS)
library(plyr)
library(caret)
library(xgboost)
library(e1071)
library(kerasR)

source("removeDuplicateEntries.R")
source("createCorpus.R")
source("createdtm.R")

journaldata1<-read.csv(file="26aijournals.csv") #read the input data file
journaldata2<-read.csv(file="44infsysjournals.csv")
journaldata3<-read.csv(file="33compsciappljournals.csv")
journaldata4<-read.csv(file="20signalprocjournals.csv")

#Remove duplicate entries from other scrapings
journaldata<-c()
journaldata<-removeDuplicateEntries(journaldata1,journaldata2)
journaldata<-removeDuplicateEntries(journaldata,journaldata3)
journaldata<-removeDuplicateEntries(journaldata,journaldata4)


nf<-10 #Number of Folds
accuracyLDA<-c()
accuracyLDApr<-c()
accuracyXG<-c()
accuracyXGwt<-c()

#Frequency of journals' papers
papers<-count(journaldata,'journal')
#If number of papers are < 100, ignore that journal - to remove insignificant journals
papers<-papers[papers$freq>100,]
weights<-max(papers$freq)/papers$freq #Weigh the frequency of papers invesely
papers<-cbind(papers,weights) #Add the weights column
#rownames(papers)<-1:length(papers$journal)
#papers$journal<-factor(papers$journal)
journaldata<-join(journaldata,papers,by = "journal",type="inner")
weights<-journaldata$weights #extract weights
journaldata<-dplyr::select(journaldata,c(-freq,-weights)) #remove column 'freq' and 'weights'
journaldata$journal<-factor(journaldata$journal) #change the levels to accomodate removed (papers from) journals

#Read text data from the data scraped from websites
print('Reading text from scraped data')
source("readTextData.R")

#Divide data into train/test set - stratified
train.index <- createDataPartition(journaldata$journal, p = .8, list = FALSE)
Trtextdata <- textdata[ train.index]
TrLabel<-journaldata$journal[train.index]
Tstextdata  <- textdata[-train.index]
TsLabel<-journaldata$journal[-train.index]

if(sum(classes!=unique(TrLabel))!=0) {
  print("Problem in class labels.")
}

#Create Train Corpus
print('Creating Training Corpus')
corpus<-getcorpus(Trtextdata)

#Create document-term frequency
print('Creating Document Term Frequency')
dtms<-getdtm(corpus)

print('LDA')
#ldadtms<-lda(dtms,TrLabel)

print("MLP _ Keras")
mod <- Sequential()

mod$add(Dense(units = 500, input_shape = dim(dtms)[2]))
mod$add(LeakyReLU())
mod$add(Dropout(0.25))

mod$add(Dense(units = 250))
mod$add(LeakyReLU())
mod$add(Dropout(0.5))

#mod$add(Dense(units = 100))
#mod$add(LeakyReLU())
#mod$add(Dropout(0.25))

mod$add(Dense(length(classes)))
mod$add(Activation("softmax"))

keras_compile(mod,  loss = 'categorical_crossentropy', metric='categorical_accuracy', optimizer = RMSprop())
trainLabel<-to_categorical(TrLabel)
trainLabel<-trainLabel[,-1]
callbacks <- list(CSVLogger(tempfile()),
                  EarlyStopping(),
                  ReduceLROnPlateau(),
                  TensorBoard(tempfile()))
keras_fit(mod, as.matrix(dtms), trainLabel, batch_size = 50, epochs = 10, verbose = 1,
          validation_split = 0.1)


#print('LDA Equal Priors')
#ldaprior<-lda(dtms,TrLabel,prior=rep(1/length(classes),length(classes)))

#print('Weighted XG boost')
#xgwt<-xgboost(as.matrix(dtms),TrLabel,nrounds=20,objective="multi:softmax",num_class=length(unique(TrLabel)),weight = Trweights)

#print('XG boost')
#xg<-xgboost(as.matrix(dtms),TrLabel,nrounds=20,objective="multi:softmax",num_class=length(unique(TrLabel)))

print("Naive Bayes")
nb<-naiveBayes(dtms,TrLabel)

#Create testset
print('Evaluating Testset')
#ind<-folds$subsets[folds$which==i] #get all testing indexes
#Tstextdata<-NULL
# pLabelXGwt<-factor(levels = sort(classes))
# pLabelXG<-factor(levels = sort(classes))
# pLabelLDA<-factor(levels = sort(classes))
# pLabelLDApr<-factor(levels = sort(classes))
pLabelNB<-factor(levels = sort(classes))
pLabelNN<-matrix(0,length(Tstextdata),length(classes))
#TsLabel<-factor(levels = sort(classes))
testLabel<-to_categorical(TsLabel)
testLabel<-testLabel[,-1]
for (j in 1:length(Tstextdata)) {
#  Tstextdata<-textdata[ind[j]]
#  TsLabel[j]<-label[ind[j]]-1
  #Create Corpus
  Tscorpus<-NULL
#  source("createCorpus.R")
  Tscorpus<-getcorpus(Tstextdata[j])
  
  #Create document term matrix
  Tsdtms<-NULL
#  source("createdtm.R")
  Tsdtms<-getdtm(Tscorpus)
  
  train.col <- colnames(dtms[,1:ncol(dtms)]) # training
  test.col <- colnames(Tsdtms[,1:ncol(Tsdtms)]) # testing
  xx <- data.frame(Tsdtms[,intersect(test.col,train.col)])
  colnames(xx)<-intersect(test.col,train.col)
  names(xx) <- gsub(".", "", names(xx), fixed = TRUE) #Remove any dots that may be appended to a keyword that matches R keywords
  # make an empty data frame with the colnames of the training data
  #yy <- read.table(textConnection(''), col.names = colnames(dtms))
  yy <- data.frame(matrix(ncol = length(dtms), nrow = 0))
  colnames(yy) <- colnames(dtms)
  zz <- rbind.fill(yy,xx)
  zz[is.na(zz)] <- 0
  #Predict test sample
  #Naive Bayes
  pLabelNB[j]<-predict(nb,zz)
  pLabelNN[j]<- keras_predict_classes(mod, as.matrix(zz))
  #LDA
  #pLabelLDA[j]<-predict(ldadtms,zz)$class
  #LDA Equal Prior
  #pLabelLDApr[j]<-predict(ldaprior,zz)$class
  #Weighted XG
  #pLabelXGwt[j]<-predict(xgwt,as.matrix(zz))
  # XG
  #pLabelXG[j]<-predict(xg,as.matrix(zz))
}
cmNB<-confusionMatrix(pLabelNB,TsLabel)
x<-pLabelNN
x<-x[,1]+1
y<-classes[x]
y<-factor(y, levels = levels(TsLabel))
cmNN<-confusionMatrix(y,TsLabel)

#cmLDA<-confusionMatrix(pLabelLDA,TsLabel)
#accuracyLDA[i]<-cmLDA$overall[1]
#print(paste0("AccuracyLDA=",accuracyLDA[i]))

# cmLDApr<-confusionMatrix(pLabelLDApr,TsLabel)
# accuracyLDApr[i]<-cmLDApr$overall[1]
# print(paste0("AccuracyLDAprior=",accuracyLDApr[i]))

# cmXGwt<-confusionMatrix(pLabelXGwt,TsLabel)
# accuracyXGwt[i]<-cmXGwt$overall[1]
# print(paste0("AccuracyXGwt=",accuracyXGwt[i]))

cmXG<-confusionMatrix(pLabelXG,TsLabel)
accuracyXG[i]<-cmXG$overall[1]
print(paste0("AccuracyXG=",accuracyXG[i]))
print('stop');
#}#end of folds
meanLDA<-mean(accuracyLDA)
meanLDApr<-mean(accuracyLDApr)
meanXG<-mean(accuracyXG)
meanXGwt<-mean(accuracyXGwt)

#Predict
# print('hi')
# for (i in 1:length(dtms)) {
#   if (colnames(zz[i]) != dimnames(ldadtms$means)[[2L]][i])
#     print(paste0("i=",i, ", zz=",colnames(zz[i]), ", dtms=",dimnames(ldadtms$means)[[2L]][i]))
# }

