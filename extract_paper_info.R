#Function to extract paper info
extract_paper_info<-function(vwebsite,i,j,dB,issue) {
  finaldata<-c()
  paper<-html_nodes(vwebsite,"h4 a") #title of the papers
  if(length(paper)!=0) { #if for some reason paper list is not read
    papertitle<-html_text(paper)
    paperhref<-html_attr(paper,"href")
    #Go into each of these papers and extract relevant information
    for (k in 1:length(paperhref)) {
      abstract<-NA
      highlights<-NA
      graphical.abstract<-NA
      keywords<-NA
      print(paste("journal=",i," volume=",j," issue=",issue," paper=",k))
      pwebsite<-tryCatch(read_html(paperhref[k],encoding = "UTF-8"),error=function(err) {}) #If paperhref does not exist, replace it with NULL
      if(is.null(pwebsite)==FALSE) {
        #Extract Abstract info
        inpaper<-html_nodes(pwebsite,".article-author-abstract") #abstract of paper
        #If abstract was not fetched, try another combination
        if (length(inpaper)==0) {
          inpaper<-html_nodes(pwebsite,".svAbstract") #abstract of paper
        }
        abstract_text<-html_text(inpaper)
        
        if (length(abstract_text)==0) { #no abstract
          abstract<-NA
        } else if (length(abstract_text)==1) { #only abstract
          #Extract abstract
          abstract<-gsub('Abstract|\n','',abstract_text[1])#remove special characters
        } else if(length(abstract_text==2)) { #abstrat AND, highlights OR graphical abstract
          
          if (length(grep("Highlights",abstract_text[1]))>0) {#if highlight occurs first, then next is abstract
            #Extract highlights
            highlights<-abstract_text[1]
            highlights<-gsub('Highlights|•','',highlights)#remove additional words
            
            #Extract Abstract
            abstract<-gsub('Abstract|\n','',abstract_text[2])#remove special characters
          } else if (length(grep("Abstract",abstract_text[1]))>0) {#if abstract occurs first, then next is graphical abstract
            #Extract Abstract
            abstract<-gsub('Abstract|\n','',abstract_text[1])#remove special characters
            #Extract graphical abstract
            graphical.abstract<-abstract_text[2]
            graphical.abstract<-gsub('Graphical abstract','',graphical.abstract)#remove additional words
          }
        } else if(length(abstract_text)==3) { #highlights, abstract and graphical abstract
          #Extract highlights
          highlights<-abstract_text[1]
          highlights<-gsub('Highlights|•','',highlights)#remove additional words
          
          #Extract abstract
          abstract<-gsub('Abstract|\n','',abstract_text[2])#remove special characters
          
          #Extract graphical abstract
          graphical.abstract<-abstract_text[3]
          graphical.abstract<-gsub('Graphical abstract','',graphical.abstract)#remove additional words
        } else 
          stop("***Some Error in fetching abstract -- STOP")
        
        #Extract Keywords
        inpkwd<-html_nodes(pwebsite,".keyword")
        Tkeywords<-html_text(inpkwd)
        if (length(Tkeywords)>0) {
          keywords<-gsub(";","",Tkeywords) #remove semicolon
          if (length(keywords)>1) {
            keywords<-paste(keywords,collapse = ' ') #store keywords in one list
          }
        }
        #Pack all info in one dataframe
        row<-as.data.frame(cbind(journal=as.data.frame(dB$journals[i]),volume=j,issue,paper=k,title=papertitle[k],abstract,highlights,graphical.abstract,keywords=keywords))
        names(row)[1]="journal"
        if (length(row)!=9) { #check if all fields are fetched properly
          stop("***Not all fields fetch properly")
        }
        finaldata<-bind_rows(finaldata,row) 
      } #end if paperhref is not null
    } #end for k
  } #end if paper length
  return (finaldata)
} #end of function

