
library(tidyverse)

# ggplots for the survey


plotOut <- 
  function(input, output){
    
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
    
    
    output
    
  }