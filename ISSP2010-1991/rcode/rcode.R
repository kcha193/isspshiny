
library(haven)
library(tidyverse)
library(questionr)


fullNameList <- fullNameForSelectList <- 
  datList <- datRawList <- 
  vector(mode = "list", length = 20)

names(fullNameList) <- names(fullNameForSelectList) <- 
  names(datList) <- names(datRawList) <- 1991:2010

source("rcode/getdata1991.R")

source("rcode/getdata2004.R")
source("rcode/getdata2005.R")
source("rcode/getdata2006.R")
source("rcode/getdata2007.R")
source("rcode/getdata2008.R")
source("rcode/getdata2009.R")
source("rcode/getdata2010.R")



save(fullNameList, fullNameForSelectList, 
     datList, datRawList, file = "all.RData" )