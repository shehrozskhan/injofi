library(shiny)
library(stringr)

function(input, output) {
  
  #output$html<-renderUI("Currently classify among 53 Elsevier's CS journals")
  
  data<-eventReactive(input$button, {
    
    #Check for empty abstract
    validate(
      need(input$title != "", label="Title"),
      need(input$abstract != "", label="Abstract"),
      need(str_count(input$abstract, '\\w+') >=50, "Abstract must be greater than or equal to 50 words"),
      need(input$keywords != "", label="Keywords")
     )
    
    # #Check for empty title
    # validate(
    #   need(input$title != "", label="Title")
    # )
    # 
    # #Check for empty keywords
    # validate(
    #   need(input$keywords != "", label="Keywords")
    # )
    #output$value<-renderPrint(str_count(input$abstract, '\\w+'))
    
        source("testModel4ui.R")
    recom<-testModel(input$abstract,input$title,input$keywords,input$highlights,input$graphabstract)
    recom
  })
  
  output$value <- renderTable({
     data()
  })
  
}


