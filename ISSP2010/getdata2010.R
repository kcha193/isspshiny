

library(haven)
library(tidyverse)
library(questionr)


datRaw <- read_spss("ISSP2010_Environment_III_data_set.sav")

#Fetch the names of dataset
fullName <- sapply(names(datRaw)[grepl("^q",names(datRaw) )], 
                   function(x) attr(datRaw %>% pull(x), "label"))

fullNameForSelect <- as.character(fullName)


#Keep only questions from Section A to B ####
# fullNameForSelect <- 
#   fullNameForSelect[1:grep("^E5e", fullNameForSelect)]


#Remove all the questions from the "other" options ####
# fullNameForSelect <- 
#   fullNameForSelect[!grepl("x\\. ", fullNameForSelect)]





# Fixing some questions #################################

# qresponsibility

# qtaxpayeduhealth
# 
# 
# labelTemp <- attr(datRaw$qtrustcourts, "labels")
# 
# newLabel <- c(0:10, 99)
# names(newLabel) <- c(names(labelTemp)[1], 1:9, names(labelTemp)[2:3])
# 
# attr(datRaw$qtrustcourts, "labels") <- newLabel
# 
# 
# 
# labelTemp <- attr(datRaw$qtrustcompanies, "labels")
# 
# newLabel <- c(0:10, 99)
# names(newLabel) <- c(names(labelTemp)[1], 1:9, names(labelTemp)[2:3])
# 
# attr(datRaw$qtrustcompanies, "labels") <- newLabel
# 



# Create a temp dataframe for plot only #####################################
dat <- datRaw %>% select(WEIGHT, AGE, SEX) %>%   
  rename(wgt = WEIGHT, 
         gender = SEX, 
         age = AGE) %>%   as.data.frame()

dat$Gender <- dat$Age <- character(nrow(dat))

dat$Gender <- as_factor( dat$gender)

age <- as.numeric(dat$age)

dat$Age <- factor(
  ifelse(age < 31, "18-30", 
         ifelse(age < 46, "31-45",
                ifelse(age < 61, "46-60",
                       ifelse(age < 76, "61-75", "76+")))), 
  levels = c("18-30", "31-45", "46-60","61-75", "76+"))








  




