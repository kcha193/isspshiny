
library(shiny)
library(shinydashboard)

source("getdata2017.R")
source("../Rcode/sidebarInput.R")
source("../Rcode/plotsOutput.R")
source("../Rcode/titleOutput.R")

#source("https://raw.githubusercontent.com/kcha193/isspshiny/master/ggplots.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "red",
  
  # Application title
  dashboardHeader(title = "ISSP 2017 -- Social Networks", titleWidth = 300),
  
  # Sidebar with a slider input for number of bins
  dashboardSidebar(

    sidebarInput("side")
    
  ),
  
  # Show a plot of the generated distribution
  dashboardBody(box(
    h2(textOutput("title")), 
    plotOutput("barPlot",  height = "800px"),
    width = 12,
    height = 850
  ))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  plotOut <- callModule(plotOutWeighted, "side", dat, datRaw)
  titleOut <- callModule(titleOut, "side")
  
  output$title <- renderText({ titleOut()})
  
  output$barPlot <- renderPlot({plotOut()})
  
}

# Run the application
shinyApp(ui = ui, server = server)
