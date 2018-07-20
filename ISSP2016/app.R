
library(shiny)
library(shinydashboard)

source("getdata2016.R")

source("https://raw.githubusercontent.com/kcha193/isspshiny/master/ggplots.R")


# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "blue",
  
  # Application title
  dashboardHeader(title = "ISSP 2016 -- Role of Government", titleWidth = 350),
  
  # Sidebar with a slider input for number of bins
  dashboardSidebar(
    
    selectInput(
      "varname",
      label = HTML('<font size=\"4\"> Select an item to examine </font>'),
      choices = c(Choose = '', fullNameForSelect),
      selectize = TRUE
    ),
    
    selectInput(
      "stratified",
      label = HTML('<font size=\"4\"> stratified by </font>'),
      choices = c("None", "Gender", "Age group", "Gender by Age"),
      selected = "None"
    ),
    
    br(),
    box(
      h4("Latest Update:"),
      h4("2018-07-19"),
      h4("Contact email:"),
      a("Barry Milne", href = "mailto:b.milne@auckland.ac.nz"),
      br(),
      a("Kevin Chang", href = "mailto:k.chang@auckland.ac.nz"),
      width = 12,
      background = "black"
    ), 
    br(),
    HTML("<a href=\"http://www.arts.auckland.ac.nz/en/about/our-research/research-centres-and-archives/compass.html\" target=\"_blank\"> <img src=\"compass.png\" width=\"200\" height=\"80\" /></a>")
    
  ),
  
  # Show a plot of the generated distribution
  dashboardBody(box(
    h2(textOutput("title")), 
    plotOutput("distPlot",  height = "800px"),
    width = 12,
    height = 850
  ))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  plotOut(input, output)
  
}

# Run the application
shinyApp(ui = ui, server = server)
