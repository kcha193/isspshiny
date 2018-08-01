
library(shiny)
library(shinydashboard)


# source("../Rcode/plotsOutput.R")
# source("../Rcode/sidebarInput.R")
# source("../Rcode/titleOutput.R")

load("all.RData")

source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/plotsOutput.R")
source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/sidebarInput.R")
source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/titleOutput.R")


rv <- reactiveValues(datRaw= NULL, dat = NULL)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "green",
  
  # Application title
  dashboardHeader(title = "ISSP 2010 - 1991"),
  
  # Sidebar with a slider input for number of bins
  dashboardSidebar(
    sidebarInputYear("side"), 
    h4(strong(textOutput("topic"))), 
    uiOutput("sidebarInputUI")), 
  
  # Show a plot of the generated distribution
  dashboardBody(box(
    tags$head(includeScript("google-analytics.js")),
    h2(textOutput("title")), 
    plotOutput("barPlot",  height = "800px"),
    width = 12,
    height = 850
  ))
)


# Define server logic required to draw a plot
server <- function(input, output) {
  
 
  
  topics <- read.csv("topics.csv", stringsAsFactors = FALSE)
  
  topicsOut <- callModule(topicsOut, "side", topics)
  

  sidebarInput <- 
    callModule(sidebarInputMultiYears, "side", 
               fullNameForSelectList = fullNameForSelectList,
               date = "01-08-2018")
  
  
  output$sidebarInputUI <- renderUI({ sidebarInput()})
  
  output$topic <- renderText({
    topicsOut()
  })
 
  plotOut <- callModule(plotOutMain, "side", 
                        datList = datList, 
                        datRawList = datRawList, 
                        fullNameList = fullNameList)
  
  
  titleOut <- callModule(titleOut, "side")
  
  output$title <- renderText({ titleOut()})
  
  output$barPlot <- renderPlot({plotOut()})
}

# Run the application
shinyApp(ui = ui, server = server)
