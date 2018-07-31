

library(haven)
library(tidyverse)
library(questionr)


datRaw <- read_spss("ISSP2016_FINAL.sav")

#Fetch the names of dataset
fullName <- sapply(names(datRaw)[grepl("^q",names(datRaw) )], 
                   function(x) attr(datRaw %>% pull(x), "label"))

fullNameForSelect <- as.character(fullName)


# Fixing some question titles 

#qspendmaori

fullName[names(fullName) == "qspendmaori"] <- 
  "A6k. Opinion on government spending on Maori development"


#qgovthousing

fullName[names(fullName) == "qgovthousing"] <- 
  "A7i. Should government provide decent housing for those who cannot afford it?"

#qgovtmaoriselfdet

fullName[names(fullName) == "qgovtmaoriselfdet"] <- 
  "A7p. Should government ensure Maori have opportunities to achieve greater levels of self-determination over all thing?"


#qgsuccessthreat

fullName[names(fullName) == "qgsuccessthreat"] <- 
  "A8c. How well is government doing at dealing with threats to New Zealand's security?" 



#Keep only questions from Section A to B ####
fullNameForSelect <- 
  fullNameForSelect[grepl("^[A-B]", fullNameForSelect)]


#Remove all the questions from the "other" options ####
fullNameForSelect <- 
  fullNameForSelect[!grepl("x\\. ", fullNameForSelect)]



# qbenefitchildren

labelTemp <- attr(datRaw$qbenefitchildren, "labels")

names(labelTemp)[2] <- "Maybe - it depends on the circumstances"

attr(datRaw$qbenefitchildren, "labels") <- labelTemp

# qresponsibility

labelTemp <- attr(datRaw$qresponsibility, "labels")

labelTemp <- 1:10

names(labelTemp)[1] <- names(labelTemp)[2] <- "People should take more responsibility"
names(labelTemp)[3] <- names(labelTemp)[4] <- "People should take responsibility"
names(labelTemp)[5] <- names(labelTemp)[6] <- "People and Government should take equal responsibility"
names(labelTemp)[7] <- names(labelTemp)[8] <- "Government should take responsibility"
names(labelTemp)[9] <- names(labelTemp)[10] <- "Government should take more responsibility"

attr(datRaw$qresponsibility, "labels") <- labelTemp

# qgovtmonitor

labelTemp <- attr(datRaw$qgovtmonitor, "labels")

labelTemp <- 0:10

names(labelTemp)[1] <- names(labelTemp)[2] <- "All government information should be publicly available"
names(labelTemp)[3] <- names(labelTemp)[4] <- "All government information can be publicly available"
names(labelTemp)[5] <- names(labelTemp)[6] <- names(labelTemp)[7] <- "All government information should be publicly available, but should also give Public security priority"
names(labelTemp)[8] <- names(labelTemp)[9] <- "Public security can be given priority"
names(labelTemp)[10] <- names(labelTemp)[11] <- "Public security should be given priority"

attr(datRaw$qgovtmonitor, "labels") <- labelTemp



# qtaxpayeduhealth

labelTemp <- attr(datRaw$qtaxpayeduhealth, "labels")

newLabel <- c(1:7, 9)
names(newLabel) <- c("Government should reduce taxes",
                     2:6,
                     "Government should increase taxes",
                     names(labelTemp)[3])

attr(datRaw$qtaxpayeduhealth, "labels") <- newLabel



# Create a temp dataframe for plot only ####
dat <- datRaw %>% select(wgt, Gender, age) %>% 
  rename(sex = Gender) %>%  as.data.frame()

dat$Gender <- dat$Age <- character(nrow(dat))

dat$Gender <- as_factor( dat$sex)

dat$Age <- as_factor( dat$age)



dat$X <- datRaw$qmostinfluence

#Remove the missing

#Remove the missing
if(is.numeric(dat$X)){
  if (any(attr(dat$X, "labels") %in% 99)) {
    dat <- dat %>% filter(as.numeric(X) != 99)
  } else {
    dat <- dat %>% filter(as.numeric(X) != 9)
  }
} else {
  if (any(names(attr(dat$X, "labels")) == "Can't choose")) {
    
    dat<- 
      dat %>% filter(as.character(X) != 
                      attr(dat$X, "labels")[
                        names(attr(dat$X, "labels")) == 
                          "Can't choose"])
  }
  
  
  dat<- 
    dat %>% filter(as.character(X) != 
                     attr(dat$X, "labels")[
                       names(attr(dat$X, "labels")) == 
                         "Can't choose"])
}


# 
# dat$XX <- as_factor(dat$X)
# 
# # Initialise ggplot2 object
# g <-
#   if (input$stratified == "None") {
#     ggplot(dat, aes(
#       x = XX,
#       weight = wgt,
#       group = 1
#     ))
#   } else if (input$stratified == "Gender") {
#     ggplot(dat %>% filter(!is.na(Gender)), 
#            aes(x = XX,
#                weight = wgt,
#                group = Gender
#            ))
#   } else if (input$stratified == "Age group") {
#     ggplot(dat %>% filter(!is.na(Age)), aes(
#       x = XX,
#       weight = wgt,
#       group = Age
#     ))
#   } else if (input$stratified == "Gender by Age") {
#     ggplot(dat %>% filter(!is.na(Gender), !is.na(Age)), aes(
#       x = XX,
#       weight = wgt,
#       group = interaction(Age, Gender)
#     ))
#   }
# 
# yLimits <-
#   if (input$stratified == "None") {
#     max(prop.table(wtd.table(dat$XX,
#                              weights = dat$wgt)), na.rm = TRUE) + .01
#   } else if (input$stratified == "Gender") {
#     max(prop.table(wtd.table(dat$XX, dat$Gender,
#                              weights = dat$wgt), 2), na.rm = TRUE) + .05
#   } else if (input$stratified == "Age group") {
#     max(prop.table(wtd.table(dat$XX, dat$Age,
#                              weights = dat$wgt), 2), na.rm = TRUE) + .15
#   } else if (input$stratified == "Gender by Age") {
#     max(prop.table(wtd.table(
#       dat$XX, interaction(dat$Age, dat$Gender),
#       weights = dat$wgt
#     ), 2), na.rm = TRUE) + .2
#   }
# 
# # Adding plot here
# g <- g +
#   geom_bar(aes(y = ..prop.., fill = factor(..x..)),
#            stat = "count") +
#   geom_text(
#     aes(label = scales::percent(..prop..),
#         y = ..prop..),
#     stat = "count",
#     hjust = 0,
#     size = 7,
#     fontface = "bold"
#   ) +
#   labs(y = "Percent",  x = "") +
#   expand_limits(y = yLimits) +
#   scale_y_continuous(labels = scales::percent) +
#   theme_bw() +
#   theme(text = element_text(size = 30)) +
#   guides(fill = "none")  +
#   scale_x_discrete(
#     labels = function(x)
#       str_wrap(x, width = 30)
#   ) +
#   coord_flip()



