

library(haven)
library(tidyverse)
library(questionr)


# datRaw <- read_spss("ISSP2013/ISSP2013 National Identity III weighting 20160726.sav")


datRaw <- read_spss("ISSP2013 National Identity III weighting 20160726.sav")

#Fetch the names of dataset
fullName <- sapply(names(datRaw)[grepl("^k",names(datRaw) )], 
                   function(x) attr(datRaw %>% pull(x), "label"))

abb <- names(fullName)

fullName <- as.character(fullName)

#Fixed A3 
fullName <- 
  str_replace(fullName, 
              "(^A3[a-z].)", 
              "\\1 How much do you agree that")


#Fixed A5 
fullName <- 
  str_replace(fullName, 
              "(^A5.)", 
              "\\1 Do you")

#Fixed A6 
fullName <- 
  str_replace(fullName, 
              "(^A6.)", 
              "\\1 Do you")


#Fixed A7 
fullName <- 
  str_replace(fullName, 
              "(^A7ai.)", 
              "\\1 How much do you agree that")

fullName <- 
  str_replace(fullName, 
              "(^A7aii.)", 
              "\\1 How much do you agree that")

fullName <- 
  str_replace(fullName, 
              "(^A7bi.)", 
              "\\1 How much do you agree that")

fullName <- 
  str_replace(fullName, 
              "(^A7bii.)", 
              "\\1 How much do you agree that")

fullName <- 
  str_replace(fullName, 
              "(^A7biii.)", 
              "\\1 How much do you agree that")

fullName <- 
  str_replace(fullName, 
              "(^A7biv.)", 
              "\\1 How much do you agree that")

#Fixed B1 and B2
fullName <- 
  str_replace(fullName, 
              "(^B1[a-z].)", 
              "\\1 How much do you agree that")

fullName <- 
  str_replace(fullName, 
              "(^B2[a-z].)", 
              "\\1 How much do you agree that")


#Fixed C1 and C2 and C3
fullName <- 
  str_replace(fullName, 
              "(^C1[a-z].)", 
              "\\1 How much do you agree that")

fullName <- 
  str_replace(fullName, 
              "(^C2.)", 
              "\\1 Do you")

fullName <- 
  str_replace(fullName, 
              "(^C3[a-z].)", 
              "\\1 How much do you agree that")

fullName <- 
  str_replace(fullName, 
              "(^C4.)", 
              "\\1 Do you think")

fullName <- 
  str_replace(fullName, 
              "(^C5.)", 
              "\\1 Do you think")


#Fixed D2
fullName <- 
  str_replace(fullName, 
              "(^D2[a-z].)", 
              "\\1 How much do you agree that")


#Fixed E1-5
fullName <- 
  str_replace(fullName, 
              "(^E1[a-z].)", 
              "\\1 How much do you agree that")

fullName <- 
  str_replace(fullName, 
              "(^E2[a-z].)", 
              "\\1 How much do you agree that")

fullName <- 
  str_replace(fullName, 
              "(^E3[a-z].)", 
              "\\1 How much do you agree that")

fullName <- 
  str_replace(fullName, 
              "(^E4[a-z].)", 
              "\\1 How much do you agree that")

fullName <- 
  str_replace(fullName, 
              "(^E5[a-z].)", 
              "\\1 How much do you agree that")


names(fullName) <- abb

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






  




