

library(haven)
library(tidyverse)
library(questionr)


datRaw <- read_spss("ISSP2017_FINAL.sav")

#Fetch the names of dataset
fullName <- sapply(names(datRaw)[grepl("^q",names(datRaw) )], 
                   function(x) attr(datRaw %>% pull(x), "label"))

fullNameForSelect <- as.character(fullName)


#Keep only questions from Section A to B ####
fullNameForSelect <- 
  fullNameForSelect[1:grep("^Q33", fullNameForSelect)]


#Remove all the questions from the "other" options ####
fullNameForSelect <- 
  fullNameForSelect[!grepl("x\\. ", fullNameForSelect)]





# Fixing some questions #################################

# qresponsibility

labelTemp <- attr(datRaw$qtrustcourts, "labels")

newLabel <- c(0:10, 99)
names(newLabel) <- c(names(labelTemp)[1], 1:9, names(labelTemp)[2:3])

attr(datRaw$qtrustcourts, "labels") <- newLabel


# qtaxpayeduhealth
labelTemp <- attr(datRaw$qtrustcompanies, "labels")

newLabel <- c(0:10, 99)
names(newLabel) <- c(names(labelTemp)[1], 1:9, names(labelTemp)[2:3])

attr(datRaw$qtrustcompanies, "labels") <- newLabel



# qsiblingcontact

labelTemp <- attr(datRaw$qsiblingcontact, "labels")


names(labelTemp)[1] <- "Not present"
names(labelTemp)[2] <- "Live together"

attr(datRaw$qsiblingcontact, "labels") <- labelTemp


# qsiblingcontact

labelTemp <- attr(datRaw$qparentcontact, "labels")

names(labelTemp)[1] <- "Not present"
names(labelTemp)[2] <- "Live together"

attr(datRaw$qparentcontact, "labels") <- labelTemp



# Create a temp dataframe for plot only #####################################
dat <- datRaw %>% select(wgt, gender, age) %>%   as.data.frame()

dat$Gender <- dat$Age <- character(nrow(dat))

dat$Gender <- as_factor( dat$gender)

dat$Age <- as_factor( dat$age)






  




