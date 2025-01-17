
library(shiny)
library(shinydashboard)

source("getdata2015.R")

source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/plotsOutput.R")
source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/sidebarInput.R")
source("https://raw.githubusercontent.com/kcha193/isspshiny/master/Rcode/titleOutput.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "yellow",
  
  # Application title
  dashboardHeader(title = "ISSP 2014/2015 - Citizenship and Work Orientation",
                  titleWidth = 500),
  
  # Sidebar with a slider input for number of bins
  dashboardSidebar(sidebarInput("side",
           date = "8-02-2019")), # <- change this for every update 
  
  
  # Show a plot of the generated distribution
  dashboardBody(tags$head(includeScript("google-analytics.js")),
                box(
                  h2(textOutput("title")),
                  plotOutput("barPlot",  height = "800px"),
                  width = 12,
                  height = 850
                ))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  plotOut <- callModule(plotOutWeighted2015, "side", 
                        dat = dat, datRaw = datRaw, 
                        qmoveTempOverall, qmoveTempByAge, 
                        qmoveTempBySex, qmoveTempBySexAge)
  
  titleOut <- callModule(titleOut, "side")
  
  output$title <- renderText({ titleOut()})
  
  output$barPlot <- renderPlot({plotOut()})
  
}

# Run the application
shinyApp(ui = ui, server = server)
