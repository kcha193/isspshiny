

library(haven)
library(tidyverse)
library(questionr)


datRaw <- read_spss("ISSP2015_FINAL.sav")

#Fetch the names of dataset
fullName <- sapply(names(datRaw)[grepl("^q",names(datRaw) )], 
                   function(x) attr(datRaw %>% pull(x), "label"))

#Remove the questions C2, C2b

fullName <- fullName[!names(fullName) %in%
                       c("qlastjobendyear", "qlastjobendmonth")]

fullNameForSelect <- as.character(fullName)


#Keep only questions from Section A to F ####
fullNameForSelect <- 
  fullNameForSelect[grepl("^[A-F]", fullNameForSelect)]


#Remove all the questions from the "other" options ####
fullNameForSelect <- 
  fullNameForSelect[!grepl("x\\. ", fullNameForSelect)]

#Remove the occupation question
fullNameForSelect <- fullNameForSelect[-182]



##################################################################
# Fixing the qimpcanhelp variable 
qimpcanhelpTemp <- as.numeric(datRaw$qimpcanhelp)

qimpcanhelpTemp[qimpcanhelpTemp == 22] <- 2

attributes(qimpcanhelpTemp) <- attributes(datRaw$qimpcanhelp)

datRaw$qimpcanhelp <- qimpcanhelpTemp


# POLITICAL WING / VOTING
qpolwingTemp <-  datRaw$qpolwing 

qpolwingTemp <- ifelse(qpolwingTemp <= 2, 0, 
                       ifelse(qpolwingTemp <= 7, 5, 10))

attributes(qpolwingTemp) <- attributes(datRaw$qpolwing)

datRaw$qpolwing <- qpolwingTemp


datRaw$qworkhours[datRaw$qworkhours==11] <- 1


# Question F6:  ####

varMove <- names(fullName[fullName %in%
                            fullNameForSelect[grepl("F6",
                                                    fullNameForSelect)]])

qmoveTemp <- 
  datRaw[, c("sex", "age", "wgt", varMove[-length(varMove)])]


qmoveTemp$Gender <- as_factor(qmoveTemp$sex)
qmoveTemp$Age <- as_factor(qmoveTemp$age)
  
qmoveTemp <- qmoveTemp %>% select(-sex, -age)


qmoveTemp[is.na(qmoveTemp)] <- 0

qmoveTempOverall <- 
  qmoveTemp %>%
  select(-Gender, - Age) %>% 
  summarise_at(vars(matches("q")), funs(prop.table(wtd.table(., weights = wgt))[2])) %>% 
  gather(key = Group, value = Prop)


qmoveTempOverall$Group <- factor(qmoveTempOverall$Group, levels = unique(qmoveTempOverall$Group))

qmoveTempByAge <- 
  qmoveTemp %>%
  select(-Gender) %>% 
  group_by(Age) %>% 
  summarise_at(vars(matches("q")), funs(prop.table(wtd.table(., weights = wgt))[2])) %>% 
  gather(key = Group, value = Prop, -Age)


qmoveTempByAge$Group <- factor(qmoveTempByAge$Group, levels = unique(qmoveTempByAge$Group))


qmoveTempBySex <- 
  qmoveTemp %>%
  select(-Age) %>% 
  group_by(Gender) %>% 
  summarise_at(vars(matches("q")), funs(prop.table(wtd.table(., weights = wgt))[2])) %>% 
  gather(key = Group, value = Prop, -Gender)


qmoveTempBySex$Group <- factor(qmoveTempBySex$Group, levels = unique(qmoveTempBySex$Group))


qmoveTempBySexAge <- 
  qmoveTemp %>%
  group_by(Gender, Age) %>% 
  summarise_at(vars(matches("q")), funs(prop.table(wtd.table(., weights = wgt))[2])) %>% 
  gather(key = Group, value = Prop, -Gender, -Age)

qmoveTempBySexAge$Group <- factor(qmoveTempBySexAge$Group, levels = unique(qmoveTempBySexAge$Group))

levels(qmoveTempOverall$Group) <- levels(qmoveTempByAge$Group) <- 
  levels(qmoveTempBySex$Group) <- levels(qmoveTempBySexAge$Group) <- 
  gsub("F6. Reasons for moving to another country: ", "", 
       fullNameForSelect[grepl("F6", fullNameForSelect)])[-length(varMove)]


fullNameForSelect <- fullNameForSelect[!grepl("F6", fullNameForSelect)]


fullNameForSelect <- c(fullNameForSelect, "F6. Reasons for moving to another country")


# Create a temp dataframe for plot only ####
dat <- datRaw %>% select(wgt, sex, age) %>% as.data.frame()

dat$Gender <- dat$Age <- character(nrow(dat))

dat$Gender <- as_factor( dat$sex)

dat$Age <- as_factor( dat$age)






  




