
library(haven)
library(tidyverse)
library(questionr)


fullNameList <- fullNameForSelectList <- 
  datList <- datRawList <- 
  vector(mode = "list", length = 20)

names(fullNameList) <- names(fullNameForSelectList) <- 
  names(datList) <- names(datRawList) <- 1991:2010

source("rcode/getdata1991.R")
source("rcode/getdata1992.R")
source("rcode/getdata1993.R")
source("rcode/getdata1994.R")
source("rcode/getdata1995.R")
source("rcode/getdata1996.R")
source("rcode/getdata1997.R")
source("rcode/getdata1998.R")
source("rcode/getdata1999.R")
source("rcode/getdata2000.R")
source("rcode/getdata2001.R")
source("rcode/getdata2002.R")
source("rcode/getdata2003.R")
source("rcode/getdata2004.R")
source("rcode/getdata2005.R")
source("rcode/getdata2006.R")
source("rcode/getdata2007.R")
source("rcode/getdata2008.R")
source("rcode/getdata2009.R")
source("rcode/getdata2010.R")



save(fullNameList, fullNameForSelectList, 
     datList, datRawList, file = "all.RData" )