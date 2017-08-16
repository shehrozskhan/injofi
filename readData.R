#Read data from sciencedirect
#library(RSelenium)
library(rvest)
library(dplyr)
source("extract_paper_info.R")

#Reading from ScienceDirect
sdurl<-"http://www.sciencedirect.com/science/journals/sub/artintell"
website<-read_html(sdurl, encoding = "UTF-8")
sciencedirect<-html_nodes(website,"span a")
journals<-html_text(sciencedirect)
#Extract correponding links for journals
href<-html_attr(sciencedirect,"href")
dB<-data.frame(journals=journals,href=href)

badString=c("this blog post", "Forgotten username or password?")

#Find the index of all bad strings in dB
index=c()
for (i in 1:length(badString)) {
  temp<-grep(badString[i],dB[,1])
  index<-append(index,temp)
}
#Remove those index, to only have journals and their links in the dB
dB<-dB[-c(index),]

#Take first 10 journals
numJournals<-10#nrow(dB)
print(numJournals)
sdurl<-"http://www.sciencedirect.com"
#badTitles=c("A")
finaldata<-c()
for (i in 1:numJournals){
  #open the journal
  url<-paste0(sdurl,dB$href[i])
  website<-read_html(url,encoding = "UTF-8")
  sciencedirect<-html_nodes(website,"div h2") #Read Volume information
  info<-html_text(sciencedirect)
  m <- gregexpr('[0-9]+',info) #Parse for Volumes
  volumes<-as.numeric(unlist(regmatches(info,m))[1]) #First number in the text should be volumne number
  
  #Go into each volume
  for (j in 1:volumes) {
    volurl<-paste0(url,'/',j)
    vwebsite<-tryCatch(read_html(volurl,encoding = "UTF-8"),error=function(err) {}) #If contents in volume does not exist, replace it with NULL
    if(is.null(vwebsite)==FALSE){ #If volume contents are present
      #read issues information
      issues<-NA
      isd<-html_nodes(vwebsite,"div h2")
      iinfo<-html_text(isd)
      #If the issue word appears, then find number of issues, and use issues information
      if (length(grep("Issue",iinfo))>0) {
        im<-gregexpr('[0-9]+',iinfo) #Parse for Issues
        issues<-as.numeric(unlist(regmatches(iinfo,im))[2]) #Second number in the text should be issue number
        for (k in 1:issues) {
          iurl<-paste0(volurl,'/',k)
          iwebsite<-tryCatch(read_html(iurl,encoding = "UTF-8"),error=function(err) {}) #If contents in issue url does not exist, replace it with NULL
          if(is.null(iwebsite)==FALSE) {#if issue contents are present
            row<-extract_paper_info(iwebsite,i,j,dB,k)
            finaldata<-bind_rows(finaldata,row)      
          }
        }
      } else {#If issue word does not appear, then use the volume information
        row<-extract_paper_info(vwebsite,i,j,dB,issues)
        finaldata<-bind_rows(finaldata,row)      
      }
    } #end for if volume contents are present
  } #end for j, volumes in a journal
} #end for i, journals

#Remove those records with no abstracts
index<-which(is.na(finaldata[,6]))
journalData<-finaldata[-c(index),]
write.table(journalData,file = "aijournals.csv", sep=",",row.names = FALSE)

# #start RSelenium
# checkForServer()
# startServer()
# remDr <- remoteDriver(browserName = 'firefox')
# remDr$open()
