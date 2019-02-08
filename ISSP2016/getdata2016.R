

library(haven)
library(tidyverse)
library(questionr)


#datRaw <- read_spss("ISSP2016/ISSP2016_FINAL.sav")

datRaw <- read_spss("ISSP2016_FINAL.sav")

#Fetch the names of dataset
fullName <- sapply(names(datRaw)[grepl("^q",names(datRaw) )], 
                   function(x) attr(datRaw %>% pull(x), "label"))



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


#qgovtapph

fullName[names(fullName) == "qgovtapph"] <- 
  "A21b. Should authorities have the right to tap people's telephone conversations?" 


#Remove qrefanythingelse

fullName <- fullName[!as.character(names(fullName)) %in% "qrefanythingelse"]


#Fixed names in A11

fullName[grep("^A11", fullName)] <- 
  "A11. Is the government's transferring responsibility for some state housing to non-government agencies an appropriate was an appropriate way to manage social housing for low-income New Zealanders?"

fullName[grep("^A14", fullName)] <- 
  "A14. Should government require working-age benefit recipients to meet work-related obligations with financial penalties for non-compliance?"

fullName[grep("^A15", fullName)] <- 
  "A15. Should government require benefit recipients with children to meet social obligations with financial penalties for non-compliance?"

fullName[grep("^A19", fullName)] <- 
  "A19. Should all government information be publicly available or should public security be given priority where this meant limiting access to government information?"

fullName[grep("^A29", fullName)] <- 
  paste0(fullName[grep("^A29", fullName)], "ion in society?")

fullName[grep("^A33", fullName)] <- 
  paste0(fullName[grep("^A33", fullName)], " return for a service?")

fullName[grep("^B3d", fullName)] <- 
  paste0(fullName[grep("^B3d", fullName)], "?")

fullName[grep("^B4d", fullName)] <- 
  paste0(fullName[grep("^B4d", fullName)], "cument?")


fullNameForSelect <- as.character(fullName)

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





