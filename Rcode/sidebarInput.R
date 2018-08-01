


# Module UI function
sidebarInput <- function(id, date = "01-08-2018") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  dafaultDate  <- as.Date("01-08-2018", "%d-%m-%Y")
  
  
  date <- as.Date(date, "%d-%m-%Y")
  
  date <- ifelse(date >  dafaultDate, 
                 format(date, "%d-%m-%Y") ,
                 format(dafaultDate, "%d-%m-%Y"))

  tagList(
    selectInput(
      ns("varname"),
      label = HTML('<font size=\"4\"> Select an item to examine </font>'),
      choices = c(Choose = '', fullNameForSelect),
      selectize = TRUE
    ),
    selectInput(
      ns("stratified"),
      label = HTML('<font size=\"4\"> stratified by </font>'),
      choices = c("None", "Gender", "Age group", "Gender by Age"),
      selected = "None"
    ),
    box(
      h4("To access the questionaire and data, go to "),
      a("Here.", href = "https://doi.org/10.17608/k6.auckland.c.2174592.v9"),
      h4("Latest Update:"),
      h4(date),
      h4("Contact email:"),
      h5(a("Barry Milne", href = "mailto:b.milne@auckland.ac.nz")),
      h5(a("Kevin Chang", href = "mailto:k.chang@auckland.ac.nz")),
      h5(a("Martin von Randow", href = "mailto:m.vonrandow@auckland.ac.nz")),
      width = 12,
      background = "black"
    ), 
    HTML("<a href=\"http://www.arts.auckland.ac.nz/en/about/our-research/research-centres-and-archives/compass.html\" target=\"_blank\"> 
         <img src=\"compass.png\" width=\"200\" height=\"95\" /></a>")
  )
}


# Module UI function
sidebarInputMultiYears <- function(id, year = 2010, date = "01-08-2018") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  dafaultDate  <- as.Date("01-08-2018", "%d-%m-%Y")
  
  
  date <- as.Date(date, "%d-%m-%Y")
  
  date <- ifelse(date >  dafaultDate, 
                 format(date, "%d-%m-%Y") ,
                 format(dafaultDate, "%d-%m-%Y"))
  
  #Read-in data for multiple years 
  source(paste0("rcode/getdata", year,".R"))
  
  
  tagList(
    selectInput(
      ns("varname"),
      label = HTML('<font size=\"4\"> Select an item to examine </font>'),
      choices = c(Choose = '', fullNameForSelect),
      selectize = TRUE
    ),
    selectInput(
      ns("stratified"),
      label = HTML('<font size=\"4\"> stratified by </font>'),
      choices = c("None", "Gender", "Age group", "Gender by Age"),
      selected = "None"
    ),
    box(
      h4("To access the datasets and questionnaires, go to ",  
         a("here.", href = "https://doi.org/10.17608/k6.auckland.c.2174592.v9")),
      h4("Latest Update:"),
      h4(date),
      h4("Contact email:"),
      h5(a("Barry Milne", href = "mailto:b.milne@auckland.ac.nz")),
      h5(a("Kevin Chang", href = "mailto:k.chang@auckland.ac.nz")),
      h5(a("Martin von Randow", href = "mailto:m.vonrandow@auckland.ac.nz")),
      width = 12,
      background = "black"
    ), 
    HTML("<a href=\"http://www.arts.auckland.ac.nz/en/about/our-research/research-centres-and-archives/compass.html\" target=\"_blank\"> 
         <img src=\"compass.png\" width=\"200\" height=\"95\" /></a>")
  )
}