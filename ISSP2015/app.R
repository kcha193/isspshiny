
library(shiny)
library(shinydashboard)

source("getdata2015.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "yellow",
  
  
  # Application title
  dashboardHeader(title = "ISSP 2015 -- Citizenship and Work Orientation", 
                
                  titleWidth = 450),
  
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
  dashboardBody(tags$head(includeScript("google-analytics.js")),
                box(
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
    
    if(req(input$varname) == "F6. Reasons for moving to another country") {
      g <-  
        if(input$stratified == "None") { 
          ggplot(qmoveTempOverall, aes(x= Group, y = Prop, fill = Group))
        } else if(input$stratified == "Gender") {  
          ggplot(qmoveTempBySex, aes(x= Group, y = Prop, fill = Group))
        } else if(input$stratified == "Age group"){  
          ggplot(qmoveTempByAge, aes(x= Group, y = Prop, fill = Group))
        } else if(input$stratified == "Gender by Age"){
          ggplot(qmoveTempBySexAge, aes(x= Group, y = Prop, fill = Group))
        }
      
      yLimits <- 
        if(input$stratified == "None") { 
          max(qmoveTempOverall$Prop, na.rm = TRUE) + .01
        } else if(input$stratified == "Gender") {  
          max(qmoveTempBySex$Prop, na.rm = TRUE) + .05
        } else if(input$stratified == "Age group"){  
          max(qmoveTempByAge$Prop, na.rm = TRUE) + .1
        } else if(input$stratified == "Gender by Age"){
          max(qmoveTempBySexAge$Prop, na.rm = TRUE) + .15
        }
      
      
      g <- g +  
        geom_col() +
        geom_text(aes(label = scales::percent(Prop)),
                  hjust = 0, size = 7, 
                  fontface = "bold") +
        expand_limits(y =yLimits)
      
    } else {
      dat$X <- datRaw %>%
        pull(names(fullName[fullName == req(input$varname)]))
      
      #Remove the missing 
      dat <- dat %>% filter(as.numeric(X)!=9)
      
      dat$XX <- as_factor(dat$X) 

      
      # Initialise ggplot2 object 
      g <-  
        if(input$stratified == "None") { 
          ggplot(dat, aes(x= XX, weight = wgt, group = 1)) 
        } else if(input$stratified == "Gender") {  
          ggplot(dat, aes(x= XX, weight = wgt, group=Gender))
        } else if(input$stratified == "Age group"){  
          ggplot(dat, aes(x= XX, weight = wgt,  group=Age)) 
        } else if(input$stratified == "Gender by Age"){
          ggplot(dat, aes(x= XX, weight = wgt, 
                          group = interaction(Age, Gender) ))
        }
      
      
      yLimits <- 
        if(input$stratified == "None") { 
          max(prop.table(wtd.table(dat$XX, 
                                   weights = dat$wgt)), na.rm = TRUE) + .01
        } else if(input$stratified == "Gender") {  
          max(prop.table(wtd.table(dat$XX, dat$Gender,
                                   weights = dat$wgt), 2), na.rm = TRUE) + .05
        } else if(input$stratified == "Age group"){  
          max(prop.table(wtd.table(dat$XX, dat$Age,
                                   weights = dat$wgt), 2), na.rm = TRUE) + .05
        } else if(input$stratified == "Gender by Age"){
          max(prop.table(wtd.table(dat$XX, interaction(dat$Age, dat$Gender),
                                   weights = dat$wgt), 2), na.rm = TRUE) + .1
        }
      
      
      # Adding plot here 
      g <- g +  
        geom_bar(aes(y = ..prop.., fill = factor(..x..)), 
                 stat= "count") +
        geom_text(aes(label = scales::percent(..prop..),
                      y= ..prop.. ),
                  stat= "count", hjust =0, size = 7, 
                  fontface = "bold")+
        expand_limits(y =yLimits)
    }
    
    
    g <- g +  
      labs(y = "Percent",  x = "") +
      scale_y_continuous(labels = scales::percent)+
      theme_bw() +
      theme(text = element_text(size=30)) +
      guides(fill= "none")  +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) + 
      coord_flip()
    
    
    
    #Facet here 
    finalPlot <- 
      if (input$stratified == "None") {
        g
      } else if (input$stratified == "Gender") {
        g + facet_grid( ~ Gender)+
          theme(text = element_text(size=25))
      } else if (input$stratified == "Age group") {
        g + facet_grid( ~ Age) +
          theme(text = element_text(size=20))
      } else if(input$stratified == "Gender by Age"){
        g + facet_grid(Gender ~ Age) +
          theme(text = element_text(size=20))
      }
    
    
    finalPlot
  }, res = 60)
}

# Run the application
shinyApp(ui = ui, server = server)
