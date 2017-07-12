require(shiny)

fluidPage(
  
  # Application title
  titlePanel("InJoFi - Intelligent Journal Finder (v0.2)"),
  sidebarLayout(
    sidebarPanel(
      textInput("title", label = h4("Enter paper Title"), placeholder = "title"),
      textAreaInput("abstract", label = h4("Enter paper Abstract"), placeholder = "abstract", height="200px"),
      textInput("keywords", label = h4("Enter Keywords"), placeholder = "keywords"),
      textInput("highlights", label = h4("Enter Paper Highlights"), placeholder = "highlights"),
      textInput("graphabstract", label = h4("Enter Paper Graphical Abstract (text only)"), placeholder = "textual content of graphical abstract"),
      
      tags$head(
        tags$style(HTML('#button{background-color:#00BFFF}'))
      ),
      actionButton("button", HTML("<strong>Find Journals</strong>")),
      
      hr(),
       tags$div(class="header", checked=NA,
                tags$p("Currently classify among"),
                tags$a(href="http://individual.utoronto.ca/shehroz/files/journals.csv", "53 Elsevier's CS journals")
       )
    ),
    #fluidRow(column(5, tableOutput("value")))
    mainPanel(tableOutput("value"))
  )
  
)

