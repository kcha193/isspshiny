

library(haven)
library(tidyverse)
library(questionr)


datRaw <- read_spss("ISSP1992_Social_Inequality_II_data_set.sav")

#Fetch the names of dataset
fullName <- sapply(names(datRaw), 
                   function(x) attr(datRaw %>% pull(x), "label"))

fullNameForSelect <- as.character(fullName)


#Keep only questions from Section A to F ####
fullNameForSelect <-
  fullNameForSelect[2:80]

# 
# #Remove all the questions from the "other" options ####
# fullNameForSelect <- 
#   fullNameForSelect[!grepl("x\\. ", fullNameForSelect)]



# Fixing the variable #################




# Create a temp dataframe for plot only ####
dat <- datRaw %>% select(Q31_1999, Q30_1999) %>% 
  rename(age = Q30_1999, 
         sex = Q31_1999) %>% 
  as.data.frame()



dat$Gender <- dat$Age <- character(nrow(dat))

dat$Gender <- as_factor( dat$sex)


age <- as.numeric(dat$age)

dat$Age <- factor(
  ifelse(age < 31, "18-30", 
         ifelse(age < 46, "31-45",
                ifelse(age < 61, "46-60",
                       ifelse(age < 76, "61-75", "76+")))), 
  levels = c("18-30", "31-45", "46-60","61-75", "76+"))






  




