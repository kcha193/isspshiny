

library(haven)
library(tidyverse)
library(questionr)
library(knitr)

datRaw <- read_spss("ISSP2015_FINAL.sav")

varnames <- names(datRaw)


dat

kable()

max(prop.table(wtd.table(datRaw$qdiscriminated, 
                     weights = datRaw$wgt)))

max(prop.table(wtd.table(datRaw$qdiscriminated, datRaw$qgender, 
                         weights = datRaw$wgt))) + 0.5

max(prop.table(wtd.table(datRaw$qdiscriminated, datRaw$age, 
                         weights = datRaw$wgt))) + 0.5


ggplot(datRaw, aes(x= qimpcanhelp, weight = wgt)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Scale") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()


kable(prop.table(wtd.table(dat$qjobmoney, dat$qgender, 
                           weights = dat$wgt), margin = 2))

ggplot(dat, aes(x= qjobmoney, weight = wgt,  group=qgender)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Gender") +
  facet_grid(~qgender) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()


kable(prop.table(wtd.table(dat$qjobmoney, dat$age, 
                           weights = dat$wgt), margin = 2))

ggplot(dat, aes(x= qjobmoney, weight = wgt,  group=age)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Age group") +
  facet_grid(~age) +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()


datQ <- 
  dat %>% 
  select(starts_with("q"))


wtd.table(dat$qjobmoney, 
          weights = dat$wgt)


ggplot(dat, aes(x = qjobmoney, weight = wgt)) + 
  geom_bar() +
  theme_bw()


ggplot(dat, aes(x = qjobmoney, weight = wgt)) + 
  geom_bar() +
  facet_wrap(~qgender) + 
  theme_bw()

ggplot(dat, aes(x = qjobmoney, weight = wgt)) + 
  geom_bar(aes(y = ..prop..)) +
  theme_bw()


ggplot(dat, aes(x = qjobmoney, weight = wgt)) + 
  geom_bar(aes(y = ..prop.., fill = factor(qgender),
               group = factor(qgender)), position = "dodge") +
  theme_bw()


ggplot(dat, aes(x = qjobmoney, weight = wgt)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)*100, fill = factor(qgender),
               group = factor(qgender)), position = "fill") +
  theme_bw()


str(dat)

