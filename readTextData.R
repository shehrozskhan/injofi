#Read extracted data from journals
textdata<-NULL
label<-NULL
charlabel<-NULL
for (i in 1:nrow(journaldata)){
  #Extract text
  title<-as.character(journaldata$title[i])
  abstract<-as.character(journaldata$abstract[i])
  keywords<-as.character(journaldata$keywords[i])
  highlights<-as.character(journaldata$highlights[i])
  graphical.abstract<-as.character(journaldata$graphical.abstract[i])
  textdata[i]<-paste(title,abstract,keywords,highlights,graphical.abstract) #combine all the text info
  textdata[i]<-gsub("-"," ",textdata[i]) #replace dash with space
  textdata[i]<- gsub("[^[:alnum:]///' ]", "", textdata[i]) #removes encoding issues?
  label[i]<-journaldata$journal[i]
  charlabel[i]<-as.character(journaldata$journal[i])
}
classes<-unique(charlabel)
