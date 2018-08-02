

library(haven)
library(tidyverse)
library(questionr)


datRaw <- read_spss("data/ISSP1999_Social_Inequality_III_data_set.sav")

#Fetch the names of dataset
fullName <- sapply(names(datRaw)[grepl("^Q", names(datRaw) )], 
                   function(x) attr(datRaw %>% pull(x), "label"))

fullNameForSelect <- as.character(fullName)

#Keep only questions from Section A to B ####
fullNameForSelect <-
  fullNameForSelect[1:grep("R: books at home when you was 15",
                           fullNameForSelect)]


#Remove all the questions from the "other" options ####
# fullNameForSelect <- 
#   fullNameForSelect[!grepl("x\\. ", fullNameForSelect)]


# Fixing some questions #################################




# Create a temp dataframe for plot only #####################################
dat <-
  datRaw %>% select( Q31, Q30) %>%   
  rename( gender = Q31, 
          age = Q30) %>%   as.data.frame()

dat$Gender <- dat$Age <- character(nrow(dat))

dat$Gender <- as_factor( dat$gender)

age <- as.numeric(dat$age)

dat$Age <- factor(
  ifelse(age < 31, "18-30", 
         ifelse(age < 46, "31-45",
                ifelse(age < 61, "46-60",
                       ifelse(age < 76, "61-75", "76+")))), 
  levels = c("18-30", "31-45", "46-60", "61-75", "76+"))



# Save into a four lists ##################

fullNameList$`1999` <- fullName
fullNameForSelectList$`1999` <- fullNameForSelect
datRawList$`1999` <- datRaw
datList$`1999` <- dat









