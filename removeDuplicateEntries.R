removeDuplicateEntries<-function(journaldata1,journaldata2) {
  
  common<-intersect(journaldata1,journaldata2)
  if(nrow(common)>0) { #If there are common elements, then
    x<-journaldata2$journal %in% common$journal
    indexCommon<-as.numeric(rownames(journaldata2[x,]))
    journaldata2<-journaldata2[-indexCommon,] #remove them
  }
  
  journaldata<-rbind(journaldata1,journaldata2) #Combine
  
  return(journaldata)
}