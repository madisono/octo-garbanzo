library(tidyverse)
library(magrittr)
library(GGally)
library(dplyr)
library(modelr)

#Task 1- read in csv
ld.prism.pop<- read_csv('ld.prism.pop.csv')
sort(names(ld.prism.pop))
ld.prism.pop
#can use sort() to see column names

#Task 2- Summary Plot
ggpairs(ld.prism.pop,columns=c("prcp", "avtemp", "size", "cases"))

#Task 3
ld.prism.pop %<>%mutate(log10size=log10(size))
ggpairs(ld.prism.pop,columns=c("prcp","avtemp","log10size","cases"))

ld.prism.pop %<>%mutate(log10cases=log10(cases+1))
ggpairs(ld.prism.pop,columns=c("prcp","avtemp","log10size","log10cases"))
#Can't name it log10(cases+1), need to rename it log10cases
#Add the +1 to number of cases because when we transform cases into log, 
#we can't take the the log of 0

#Simple Linear Model

#Task 4
set.seed(222)
smallsample<- ld.prism.pop %>% sample_n(100)
smallsample
p<-ggplot(data=smallsample) +
  geom_point(aes(x=prcp, y=avtemp))
p
#Task 5-Want to add straight line (regression)
p<-p+geom_smooth(aes(prcp,avtemp), method="lm")
p

#Task 6-Get Summary
myModel<- lm(avtemp ~ prcp, data=smallsample)
myModel
summary(myModel)

summary(myModel)$coefficients[2,1]
summary(myModel)$coefficients[2,4]
#Calls upon the summary table and tells you certian values
#[2,1] calls the slope
#[2,4] calls the p-value

#Task 7-Slope of line plotted in Task 5: 0.00301, and is significantly 
#different because it is less than 0.05

#The Modelr Package
#Task 8
ld.prism.pop %>% group_by(year) %>% summarize(total=sum(size)) %>%
  ggplot(.)+geom_point(aes(x=year,y=total))

#Task 9
by_state<- ld.prism.pop %>% ungroup %>% group_by(state) 
by_state 

#Task 10
by_state %<>% nest
by_state
#The data is now organized by state and counties and so on are in a separate tibble by state

#Task 11- Display Georgia
by_state$data[[10]]
#Shows only the Georgia data now

#Task 12-Write a Function
linGrowth_model <- function(by_state){
  lm(size ~ year, data = by_state)
}

linGrowth_model
models <- purrr::map(by_state$data, linGrowth_model)
models

detach("package:maps", unload=TRUE)

#Task 13
by_state %<>% mutate(model = map(data, linGrowth_model))
by_state
by_state %<>% mutate(resids =map2(data, model, add_residuals))
by_state
#Task 14- Inspect Resids
#The resids are in <list> tibble format, so a data frame inside a data frame

#Task 15- Write a Function
sum_resids <- function(x){
   sum(abs(x$resid))
}
by_state %<>% mutate(totalResid = map(resids,sum_resids))

#Task 16- Slope
get_slope <- function(model){
  model$coefficients[2]
}
by_state %<>% mutate(slope = purrr::map(model, get_slope))

slopes <- unnest(by_state, slope)
totalResids <- unnest(by_state, totalResid)
slopes
totalResids

#Task 17-Plot the Growth
slopes %>% ggplot(aes(state,slope))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Task 18- Plot the total Residuals for all states
totalResids %>% ggplot(aes(state,totalResid))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Task 19- Repeat Task 9 and 10
#Task 9: Create a data frame called "by_state" from the main data frame, that groups by state, and inspect it.
by_state2<- ld.prism.pop %>% ungroup %>% group_by(state) 
by_state2 
#Task 10: Next, update this new data frame so that it is nested (simply pass it to nest). Again, inspect the
#data frame by typing its name in the console so see how things changed.
#by_state %<>% nest
#by_state
by_state2 %<>% nest
by_state2
#The data is now organized by state and counties and so on are in a separate tibble by state

#Task 20-Write a Function that accepts an element of the by_state2$data list-column and returns the
#spearman correlation coefficient between Lyme disease cases and precipitation
?cor.test

runCor <- function(df){
  suppressWarnings(cor.test(df$cases,df$prcp,method="spearman")$estimate)
}
by_state2 %<>% mutate(spCor = purrr::map(data, runCor))
by_state2
spCors <- unnest(by_state2,spCor)
spCors
spCors %<>% arrange(desc(spCor))
spCors
spCors$state <- factor(spCors$state, levels=unique(spCors$state))
ggplot(spCors,aes(state,spCor))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



















