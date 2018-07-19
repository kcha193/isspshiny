

library(haven)
library(tidyverse)
library(questionr)


datRaw <- read_spss("ISSP1991_Religion_I_data_set.sav")

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


ifelse(dat$age 


dat$Age <- factor()






  




