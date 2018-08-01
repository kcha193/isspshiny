
library(shiny)
library(shinydashboard)



source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/plotsOutput.R")
source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/sidebarInput.R")
source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/titleOutput.R")


# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "green",
  
  # Application title
  dashboardHeader(title = "ISSP 2010 - 1991", titleWidth = 280),
  
  # Sidebar with a slider input for number of bins
  dashboardSidebar(
    selectInput(
      "year",
      label = HTML('<font size=\"4\"> Select the year of survey </font>'),
      choices = c(Choose = '', 2010:1991),
      selectize = TRUE, 
      selected = "2010"
    ),
    uiOutput("sidebarInput")), 
  
  
  # Show a plot of the generated distribution
  dashboardBody(box(
    #tags$head(includeScript("google-analytics.js")),
    h2(textOutput("selYear")), 
    h2(textOutput("title")), 
    plotOutput("barPlot",  height = "800px"),
    width = 12,
    height = 850
  ))
)


# Define server logic required to draw a plot
server <- function(input, output) {
  

  output$selYear <- renderText({ input$year})
  
  
  output$sidebarInput <- renderUI({
    sidebarInputMultiYears("side", date = "25-07-2018") # <- change this for every update 
    
  })
  
  
  plotOut <- callModule(plotOutWeighted, "side", 
                        dat = dat, datRaw = datRaw)
  
  titleOut <- callModule(titleOut, "side")
  
  
  output$title <- renderText({ titleOut()})
  
  output$barPlot <- renderPlot({plotOut()})
  
}

# Run the application
shinyApp(ui = ui, server = server)
