
library(shiny)
library(shinydashboard)

source("getdata2006.R")

source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/plotsOutput.R")
source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/sidebarInput.R")
source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/titleOutput.R")


# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "blue",
  
  # Application title
  dashboardHeader(title = "ISSP 2006 - Role of Government IV", titleWidth = 370),
  
  # Sidebar with a slider input for number of bins
  dashboardSidebar(sidebarInput("side",
                                date = "26-07-2018")), # <- change this for every update 
  
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
