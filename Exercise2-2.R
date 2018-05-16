library("lubridate")
library("ggplot2")
wnv <- read.csv('https://raw.githubusercontent.com/jdrakephd/ideas-workshop/master/wnv.csv')
head(wnv)

#Exercise1
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=Total, fill=State)) +
  facet_wrap(~ Year) +
  labs(x='Total Number of WNV Cases', y='Frequency' , title='Number of WNV Cases Statewide',
       caption="Data fom: https://diseasemaps.usgs.gov/")

#Exercise2
#Samples are highly skewed, so we want the log of them to plot instead
#Log example 1
wnv$logged <- log(wnv$Total)
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=wnv$logged, fill=State)) +
  facet_wrap(~ Year) +
  labs(x='Log Number of Total Cases of WNV Cases', y='Frequency' , title='Number of WNV Cases Statewide',
       caption="Data fom: https://diseasemaps.usgs.gov/")

#Log example 2
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=log(Total), fill=State)) +
  facet_wrap(~ Year) +
  labs(x='Log Number of Total Cases of WNV Cases', y='Frequency' , title='Number of WNV Cases Statewide',
       caption="Data fom: https://diseasemaps.usgs.gov/")

#Exercise3
#Case fatality rate
cfr <- wnv$Fatal/wnv$Total
ggplot(data=wnv) +
  geom_histogram(mapping=aes(x=cfr, fill=State)) +
  facet_wrap(~ Year) +
  labs(x='Fatalities', y='Frequency' , title='Number of WNV Fatalities Statewide',
       caption="Data fom: https://diseasemaps.usgs.gov/")

#Exercise4
head(wnv)
wnv$var.total<- sum(wnv$EncephMen + wnv$Fever + wnv$Other-wnv$Total)
#All values equal 0, proving the variable total is the sum

#Exercise5-Round to nearest dozen
#Thought a Function would work...
mround <- function(x,base){ 
  base*round(x/base) 
} 
mround("Total",12) 
round("Total", multiple = 12)

#POTENTIAL SOLUTION
rounded_total <- (wnv$Total %/% 12) * 12
rounded_total
rounded_error <- sum(wnv$Total %% 12)
rounded_error

#Functions
#Exercise6
#mean <- function(x){
  #s <-sum(x)
  #n <- length(x)
  #m <- s/n
  #return(m)
  #}

mean_std_error <- function(x){
  m <- mean(x)
  std_dev <- sd(x)
  sq_rt <- sqrt(length(x))
  s_e <- std_dev/sq_rt
  return(c(m,s_e))
}
mean_std_error(c(0,3,47))

NID_rate <- function(x){
  nid <- x$EncephMen
  tot <- x$Total
  nid_r <- nid/tot
  return(nid_r)
}

NID_rate(wnv)

#assign states we want and then assign years we want

states <- c('California', "Colorado", "New York")
years<- C(2000:2004)


#Exercise7
#ggplot NID as bar plot with error bars
#Couldn't figure out original neuroinvasive disease rates for states, 
#so couldnt't create the ggplot

#Exercise8
#use function to show ggplot of NID for all states
#Again, couldn't figure out original neuroinvasive disease rates for states, 
#so couldnt't create the ggplot

#Exercise9
#use pipes to produce same plots
#Again, couldn't figure out original neuroinvasive disease rates for states, 
#so couldnt't create the ggplot

#Exercise10
ifelse(wnv$Longitude > -98.35, wnv$eastern <- wnv$Longitude, 
  wnv$western <- wnv$Longitude)

#Exercise11
#compare case fatality in East vs West
wnv$eastern <- wnv$Longitude > -98.35
wnv$eastern
wnv$western <- wnv$Longitude < -98.35
wnv$western

ggplot(data=wnv) +
  geom_dotplot(mapping=aes(x=wnv$eastern, y=wnv$western))

#Exercise12
#Is there latitudinal gradient?
#YES

#Exercise13
#Loops
times <- seq(1:10)
some.algorithm <- function(t){
  y <- t*10
}

output <- c()
for(t in times){
  output <- c(output, some.algorithm(t))
}
plot(times, output, type='p')
#Loop over all years in WNV (1999-2007)

#Total number of states reporting cases
years <- seq(1999:2007)
state.cases <- function(x){
  y <- sum(x,wnv$States)
}

output <- c()
for(x in years){
  output <- c(output, state.cases(x))
}
plot(years, output, type='p')

#Total number of cases
years <- seq(1999:2007)
total.cases <- function(x){
  y <- sum(wnv$Total)
}

output <- c()
for(x in years){
  output <- c(output, total.cases(x))
}
plot(years, output, type='p')

#Total number of fatalities
years <- seq(1999:2007)
fatal.cases <- function(x){
  y <- sum(wnv$Fatal)
}

output <- c()
for(x in years){
  output <- c(output, fatal.cases(x))
}
plot(years, output, type='p')


#Case Fatality Rate
years <- seq(1999:2007)
case.fatality <- function(x){
  y <- sum(cfr)
}

output <- c()
for(x in years){
  output <- c(output, case.fatality(x))
}
plot(years, output, type='p')
#I don't think these plots represent the data well


#TOTALS
years <- seq(1999:2007)
totals <- function(y){
  state.cases
  total.cases
  fatal.cases
  case.fatality
}

output <- c()
for(y in years){
  output <- c(output, totals(y))
}
plot(years, output, type='p')
#didn't work

#Exercise14
#How does longitiudinal breakpoint matter for case fatality?
#depending on where the line is drawn, the total fatality on each side will change
#The fatality rate seems to be more mideastern, so if the line were drawn more towrds
#the east, those fatalities would now be on the western side, changing the rate

#Exercise15
?prop.test

#Exercise16
?binom.test






