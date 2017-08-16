#Term Frequency Inverse Document Frequency
#print("Term Frequency Inverse Document Frequency")
gettfidf<-function(corpus,Label) {
  #tdm<-TermDocumentMatrix(corpus)
  tfidf<-DocumentTermMatrix(corpus, control=list(weighting=weightTfIdf, minDocFreq=1))
  #tfidf<-weightTfIdf(dtm)
  #tfidf<-removeSparseTerms(tfidf,0.99)
  #tfidf<-as.matrix(tfidf)
  #tfidf<-t(tfidf) #convert to document-term
  tfidf<-as.data.frame(tfidf)
  rownames(tfidf)<-make.names(tfidf[1:nrow(tfidf),1],unique = TRUE) #relabel unique row names in dtms
  #tfidf<-cbind(tfidf,journaldata$journal)  #add class label name
  #colnames(tfidf)[ncol(tfidf)]<-"journalName"  #rename class column
  #write.csv(tfidf,file = "tfidf.csv",row.names = FALSE)
  return(tfidf)
}