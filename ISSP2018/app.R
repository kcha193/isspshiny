
library(shiny)
library(shinydashboard)


source("getdata2018.R")


source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/plotsOutput.R")
source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/sidebarInput.R")
source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/titleOutput.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "green",
  
  # Application title
  dashboardHeader(title = "ISSP 2018 - Religion IV", titleWidth = 250),
  
  # Sidebar with a slider input for number of bins
  dashboardSidebar(sidebarInput("side",
          date = "7-2-2019")), # <- change this for every update 
  
  # Show a plot 
  dashboardBody(box(
    h2(textOutput("title")), 
    plotOutput("barPlot",  height = "800px"),
    width = 12,
    height = 850
  ))
)


# Define server logic required to draw a plot
server <- function(input, output) {
  
  plotOut <- callModule(plotOutWeighted, "side", dat, datRaw)
  titleOut <- callModule(titleOut, "side")
  
  output$title <- renderText({ titleOut()})
  
  output$barPlot <- renderPlot({plotOut()})
  
}

# Run the application
shinyApp(ui = ui, server = server)
