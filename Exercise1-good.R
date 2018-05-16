library("lubridate")
library("ggplot2")
library(plotly)
setwd('/Volumes/Madison_HD/mers')
mers <- read.csv('cases.csv')
head(mers)
class(mers$onset)
head(mers$onset, 2)
mers$hospitalized[890] <- c('2015-02-20')
mers <- mers[-471,]
mers$onset2 <- ymd(mers$onset)
mers$hospitalized2 <- ymd(mers$hospitalized)
class(mers$onset2)
day0 <- min(na.omit(mers$onset2))
#na.omit is used to get rid of columns or rows missing data/values
mers$epi.day <- as.numeric(mers$onset2 - day0)
#it converts a data frame to a new numeric matrix (number)
#This is the days since the onset- day 0 for each value in the onset column

ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day)) + 
  labs(x='Epidemic day', y='Case count' , title='Global count of MERS cases by date of symptom onset',
       caption="Data fom: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

geom_bar(mapping=aes(x=epi.day)) 
  labs(x='Epidemic day', y='Case count' , title='Global count of MERS cases by date of symptom onset',
       caption="Data fom: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#You have to do each command seperately, they are no longer stacked
  
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country)) + 
  labs(x='Epidemic day', y='Case count' , title='Global count of MERS cases by date of symptom onset',
       caption="Data fom: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") 

ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country), position="fill") + 
  labs(x='Epidemic day', y='Case count' , title='Global count of MERS cases by date of symptom onset',
       caption="Data fom: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") 
#the bars were set to 1.0 

ggplot(data=mers) + 
  geom_bar(mapping=aes(x=epi.day, fill=country)) + coord_flip() +
  labs(x='Epidemic day', y='Case count' , title='Global count of MERS cases by date of symptom onset',
       caption="Data fom: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") 

mers$infectious.period <- mers$hospitalized2-mers$onset2
class(mers$infectious.period)
mers$infectious.period <- as.numeric(mers$infectious.period, units= 'days')
ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period)) +
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period',
       caption='Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv')
mers$infectious.period2 <- ifelse(mers$infectious.period<0,0,mers$infectious.period)
ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency', 
       title='Distribution of calculated MERS infectious perdiod (positive values only)', caption="Data from: //github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplot(data=mers) +
  geom_density(mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency', 
       title='Probability density for MERS infectious perdiod (positive values only)', caption="Data from: //github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplot(data=mers) +
  geom_area(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency', 
       title='Area plot for MERS infectious perdiod (positive values only)', caption="Data from: //github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplot(data=mers) +
  geom_dotplot(mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency', 
       title='Probability density for MERS infectious perdiod (positive values only)', caption="Data from: //github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#Bivariate Plots
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=infectious.period2)) + 
  labs(x='Infectious Period 2', y='Case count' , title='Global count of MERS cases by date of symptom onset',
       caption="Data fom: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

ggplot(data=mers) + 
  geom_smooth(method=loess, size=1.5, mapping=aes(x=epi.day, y=infectious.period2)) + 
  labs(y='Infectious Period 2', x='Epi Day' , title='Global count of MERS cases by date of symptom onset',
       caption="Data fom: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") 

ggplot(data=mers) + 
  geom_point(mapping=aes(x=epi.day, y=infectious.period2,colour=country)) + 
  labs(y='Infectious Period 2', x='Epi Day' , title='Global count of MERS cases by date of symptom onset',
       caption="Data fom: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") 

ggplot(data=mers) + 
  geom_smooth(method=loess, size=1.5, mapping=aes(x=epi.day, y=infectious.period2,colour=country)) + 
  labs(y='Infectious Period 2', x='Epi Day' , title='Global count of MERS cases by date of symptom onset',
       caption="Data fom: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") 

#Faceting
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping = aes(colour=country)) +
  facet_wrap(~ country) +
  scale_y_continuous(limits = c(0,50)) +
  labs(x='Epidemic Day', y='Infectious Period', 
       title='MERS infectious period (positie values only) overtime', caption='Data from:https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv')

ggplot(data=subset(mers, gender %in% c('M', 'F') & country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'South Korea', 'UAE'))) +
  geom_point(mapping = aes(x=epi.day, y=infectious.period2, colour=country)) +
  facet_grid(gender ~ country) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic Day', y='Infectious Period', 
       title='MERS infectious period by gender and country', caption='Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv')

#Working on Fatality Rate Exercise
ggplot(data=subset(mers, gender %in% c('M', 'F') & country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'South Korea', 'UAE'))) +
  geom_bar(mapping = aes(x=death, y=infectious.period2, colour=country)) +
  facet_grid(gender ~ country) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Fatality Rate', y='Infectious Period', 
       title='MERS infectious period by gender and country', caption='Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv')

fatality <- na.omit(mers, mers$outcome)
ggplot(data=mers) + 
  geom_bar(mapping=aes(x=outcome, fill=country)) + 
  labs(x='Outcome', y='Case count' , title='Fatality Rate of MERS Across Countries',
       caption="Data fom: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#ggplot extensions
epi.curve <- ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symp' ,
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
ggplotly(epi.curve)
