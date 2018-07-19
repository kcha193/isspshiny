
library(shiny)
library(shinydashboard)

source("getdata2017.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "red",
  
  # Application title
  dashboardHeader(title = "ISSP 2017 -- Social Networks", titleWidth = 300),
  
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
      h5(a("Barry Milne", href = "mailto:b.milne@auckland.ac.nz")),
      br(),
      h5(a("Kevin Chang", href = "mailto:k.chang@auckland.ac.nz")),
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
  
  output$title <- renderText({input$varname})
  
  
  output$distPlot <- renderPlot({
    dat$X <- datRaw %>%
      pull(names(fullName[fullName == req(input$varname)]))
    
    #Remove the missing
    if(any(attr(dat$X, "labels") %in% 99)){
      dat <- dat %>% filter(as.numeric(X) != 99)
    } else {
      dat <- dat %>% filter(as.numeric(X) != 9)
    }
    
    
    dat$XX <- as_factor(dat$X)
    
    # Initialise ggplot2 object
    g <-
      if (input$stratified == "None") {
        ggplot(dat, aes(
          x = XX,
          weight = wgt,
          group = 1
        ))
      } else if (input$stratified == "Gender") {
        ggplot(dat, aes(
          x = XX,
          weight = wgt,
          group = Gender
        ))
      } else if (input$stratified == "Age group") {
        ggplot(dat, aes(
          x = XX,
          weight = wgt,
          group = Age
        ))
      } else if (input$stratified == "Gender by Age") {
        ggplot(dat, aes(
          x = XX,
          weight = wgt,
          group = interaction(Age, Gender)
        ))
      }
    
    
    yLimits <-
      if (input$stratified == "None") {
        max(prop.table(wtd.table(dat$XX,
                                 weights = dat$wgt)), na.rm = TRUE) + .01
      } else if (input$stratified == "Gender") {
        max(prop.table(wtd.table(dat$XX, dat$Gender,
                                 weights = dat$wgt), 2), na.rm = TRUE) + .05
      } else if (input$stratified == "Age group") {
        max(prop.table(wtd.table(dat$XX, dat$Age,
                                 weights = dat$wgt), 2), na.rm = TRUE) + .05
      } else if (input$stratified == "Gender by Age") {
        max(prop.table(wtd.table(
          dat$XX, interaction(dat$Age, dat$Gender),
          weights = dat$wgt
        ), 2), na.rm = TRUE) + .1
      }
    
    
    # Adding plot here
    g <- g +
      geom_bar(aes(y = ..prop.., fill = factor(..x..)),
               stat = "count") +
      geom_text(
        aes(label = scales::percent(..prop..),
            y = ..prop..),
        stat = "count",
        hjust = 0,
        size = 7,
        fontface = "bold"
      ) +
      labs(y = "Percent",  x = "") +
      expand_limits(y = yLimits) +
      scale_y_continuous(labels = scales::percent) +
      theme_bw() +
      theme(text = element_text(size = 30)) +
      guides(fill = "none")  +
      scale_x_discrete(
        labels = function(x)
          str_wrap(x, width = 50)
      ) +
      coord_flip()
    
    
    
    #Facet here
    if (input$stratified == "None") {
      g
    } else if (input$stratified == "Gender") {
      g + facet_grid(~ Gender) +
        theme(text = element_text(size = 25))
    } else if (input$stratified == "Age group") {
      g + facet_grid(~ Age) +
        theme(text = element_text(size = 20))
    } else if (input$stratified == "Gender by Age") {
      g + facet_grid(Gender ~ Age) +
        theme(text = element_text(size = 20))
    }
    
  }, res = 55)
}

# Run the application
shinyApp(ui = ui, server = server)
