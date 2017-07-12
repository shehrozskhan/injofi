#This script test an already saved model for the UI, input comes from the UI
testModel<-function(abstract,title,keywords,highlights,graphical.abstract) {

  library(tm)
  library(SnowballC)
  library(dplyr)
  library(xgboost)
  library(plyr)
    
  textdata<-paste(as.character(title),as.character(abstract),as.character(keywords),as.character(highlights),
                  as.character(graphical.abstract)) #combine all the text info
  textdata<-gsub("-"," ",textdata) #replace dash with space
  textdata<- gsub("[^[:alnum:]///' ]", "", textdata) #removes encoding issues?
  
  #Create Corpus
  Tscorpus<-NULL
  source("createCorpus.R")
  Tscorpus<-getcorpus(textdata)

  #Create document term matrix
  Tsdtms<-NULL
  source("createdtm.R")
  Tsdtms<-getdtm(Tscorpus)

  #Load train labels
  load("param")
  
  test.col <- colnames(Tsdtms) # testing
  common_col<-intersect(test.col,train.col)
  validate(
    need(length(common_col) != 0, "Your paper's information does not match with the database.")
  )
  xx <- data.frame(Tsdtms[,common_col])
  names(xx) <- gsub(".", "", names(xx), fixed = TRUE) #Remove any dots that may be appended to a keyword that matches R keywords
  colnames(xx)<-common_col
  # make an empty data frame with the colnames of the training data
  #yy <- read.table(textConnection(''), col.names = colnames(dtms))
  yy <- data.frame(matrix(ncol = length(dtms), nrow = 0))
  colnames(yy) <- colnames(dtms)
  zz <- rbind.fill(yy,xx)
  zz[is.na(zz)] <- 0

  #Load model
  xgmodel<-xgb.load('xgboost.model')
  #Predict
  prob<-predict(xgmodel,as.matrix(zz))
  sprob<-sort(prob,index.return=TRUE,decreasing = TRUE)
  recom<-data.frame(Journal=character(),Confidence=character(),stringsAsFactors = FALSE)
  for (i in 1:10) {
    row<-as.data.frame(cbind(Journal=uniqueCL[sprob$ix[i]], Confidence=format(round(sprob$x[i], 2), nsmall = 2)),stringsAsFactors = FALSE)
    recom<-bind_rows(recom,row)
  }
  
  return(recom)
}