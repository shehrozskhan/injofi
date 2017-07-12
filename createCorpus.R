#convert textwords to corpus
#corpus<-as.VCorpus(textdata)
getcorpus<- function(textdata) {
  corpus<-Corpus(VectorSource(textdata))
  corpus<-tm_map(corpus,tolower) #convert to lowercase
  corpus <- tm_map(corpus, removeNumbers) #remove numbers
  corpus <- tm_map(corpus, removeWords, stopwords("english"))   #remove stop words
  corpus<-tm_map(corpus,removePunctuation) #remove punctuations
  corpus<-tm_map(corpus,stripWhitespace) #remove whitespaces
  #corpus <- tm_map(corpus, PlainTextDocument)
  corpus<-tm_map(corpus,stemDocument) #stem words
  return(corpus)
}
