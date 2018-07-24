

library(haven)
library(tidyverse)
library(questionr)


datRaw <- read_spss("ISSP2013 National Identity III weighting 20160726.sav")

#Fetch the names of dataset
fullName <- sapply(names(datRaw)[grepl("^k",names(datRaw) )], 
                   function(x) attr(datRaw %>% pull(x), "label"))

fullNameForSelect <- as.character(fullName)


#Keep only questions from Section A to B ####
fullNameForSelect <- 
  fullNameForSelect[1:grep("^E5e", fullNameForSelect)]


#Remove all the questions from the "other" options ####
fullNameForSelect <- 
  fullNameForSelect[!grepl("x\\. ", fullNameForSelect)]





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
dat <- datRaw %>% select(kweight, kgender, kagegrp) %>%   
  rename(wgt = kweight, 
         gender = kgender, 
         age = kagegrp) %>%   as.data.frame()

dat$Gender <- dat$Age <- character(nrow(dat))

dat$Gender <- as_factor( dat$gender)

dat$Age <- as_factor( dat$age)






  




