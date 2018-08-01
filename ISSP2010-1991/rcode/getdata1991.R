

library(haven)
library(tidyverse)
library(questionr)


datRaw <- read_spss("data/ISSP1991_Religion_I_data_set.sav")

#Fetch the names of dataset
fullName <- sapply(names(datRaw)[grepl("^Q",names(datRaw) )], 
                   function(x) attr(datRaw %>% pull(x), "label"))

fullNameForSelect <- as.character(fullName)


#Keep only questions from Section A to F ####
fullNameForSelect <-
  fullNameForSelect[1:grep("^Q40", names(fullName))-1]

# 
# #Remove all the questions from the "other" options ####
# fullNameForSelect <- 
#   fullNameForSelect[!grepl("x\\. ", fullNameForSelect)]



# Fixing the variable #################




# Create a temp dataframe for plot only ####
dat <- datRaw %>% select(Q40, Q39) %>% 
  rename(age = Q39, 
         sex = Q40) %>% 
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

# Save into a four lists ##################

fullNameList$`1991` <- fullName
fullNameForSelectList$`1991` <- fullNameForSelect
datRawList$`1991` <- datRaw
datList$`1991` <- dat


  




