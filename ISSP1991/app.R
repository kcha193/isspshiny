
library(shiny)
library(shinydashboard)

source("getdata1991.R")
# source("../Rcode/sidebarInput.R")
# source("../Rcode/plotsOutput.R")
# source("../Rcode/titleOutput.R")

source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/plotsOutput.R")
source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/sidebarInput.R")
source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/titleOutput.R")


# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "black",
  
  # Application title
  dashboardHeader(title = "ISSP 2013 -- National Identity", titleWidth = 350),
  
  # Sidebar with a slider input for number of bins
  dashboardSidebar(sidebarInput("side",
                                date = "25-17-2018")), # <- change this for every update 
  
  
  # Show a plot of the generated distribution
  dashboardBody(box(
    #tags$head(includeScript("google-analytics.js")),
    h2(textOutput("title")), 
    plotOutput("barPlot",  height = "800px"),
    width = 12,
    height = 850
  ))
)


# Define server logic required to draw a plot
server <- function(input, output) {
  
  plotOut <- callModule(plotOutUnweighted, "side", 
                        dat = dat, datRaw = datRaw)
  
  titleOut <- callModule(titleOut, "side")
  
  output$title <- renderText({ titleOut()})
  
  output$barPlot <- renderPlot({plotOut()})
  
}

# Run the application
shinyApp(ui = ui, server = server)
