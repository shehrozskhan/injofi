#Term Frequency
#print("Term Frequency")
getdtm<-function(corpus) {
  dtm <- DocumentTermMatrix(corpus)  
  dtms <- removeSparseTerms(dtm, 0.99) # This makes a matrix that is x% empty space, maximum.   
  dtms<-as.data.frame(as.matrix(dtms))
  #rownames(dtms)<-make.names(dtms[1:nrow(dtms),1],unique = TRUE) #relabel unique row names in dtms
  #dtms<-cbind(dtms,Label) #add class label column
  #colnames(dtms)[ncol(dtms)]<-"journalName" #rename class column
  return(dtms)
}
