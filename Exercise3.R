library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)
library(ggplot2)
ld <- read_csv('lyme.csv')
pop <- read_csv('pop.csv')
prism <- read_csv('climate.csv')
ld
pop
prism

#Pism is the only data in tidy format, we want to clean up the others
pop %<>% select(fips, starts_with("pop2"))
pop
pop %<>% gather(starts_with("pop2"), key="str_year", value="size") %>% na.omit
pop
pop %<>% mutate(year=str_replace_all(str_year,"pop",""))
pop
pop %<>% mutate(year=as.integer(year)) 
pop
pop %<>% mutate(fips=str_replace_all(fips,"^0",""))
pop
pop %<>% mutate(fips=as.integer(fips))
pop  

#How do you remove statewide summaries at this stage?
#something to do with the the integers of the FIPS codes
#Mayve determine which fips code relates to each state and remove it

#Lyme Disease
#No Fips code, need to create one with the sate and county integers
ld %<>% gather(starts_with("Cases"),key="str_year",value="cases")
ld
ld %<>% mutate(year=str_replace_all(str_year,"Cases",""))
ld
ld %<>% mutate(year=as.integer(year))
ld
ld %<>% rename(state=STNAME,county=CTYNAME)
ld
fips.builder <- function(st,ct){
  if (str_length(ct)==3){
    fips<-paste(as.character(st),as.character(ct),sep="") %>% as.integer
  }
  else if (str_length(ct)==2){
    fips<-paste(as.character(st),"0",as.character(ct),sep="") %>% as.integer
  }
  else {
    fips<-paste(as.character(st),"00",as.character(ct),sep="") %>% as.integer
  }
  return(fips)
}
ld
ld %<>% rowwise() %>% mutate(fips=fips.builder(STCODE,CTYCODE)) 
ld
ld %<>% select(-c(STCODE,CTYCODE,str_year))
ld

?merge

#Join Lyme Disease and PRISM
ld.prism<- inner_join(ld,prism)

#Join demographic data as well
ld.prism.pop<- inner_join(ld.prism, pop)

#Obtain Summary Information
cases_by_year <- ld %>% ungroup %>% group_by(year) %>%
  summarize(total=sum(cases)) %>% arrange(desc(total))

#Average among states, county and year, for total impact
cases_by_state <- ld.prism.pop %>% ungroup %>% group_by(state) %>%
  summarize(avCases=mean(cases)) %>% arrange(desc(avCases)) 


#cases_by_state <- ld.prism.pop %>% ungroup %>% group_by(state, year) %>%
  #summarize(total=sum(cases)) %>% arrange(desc(total)) 

#Worst Year: 2009
#Worst year by state: 2008, New York (80 and 81)
#Worst states: Connecticut, Massachusetts, Delaware  

#Saving Data Frames
save(ld.prism.pop,file='ld.prism.pop.Rda')
?write_csv
write_csv(ld.prism.pop, 'ld.prism.pop.csv')



#Visualize Geographic Data
#get map data for US counties and states
county_map <- map_data("county")
state_map <- map_data("state")

ag.fips <- group_by(ld.prism.pop,fips)
#arranges the ld.prism.pop df in order of fips codes
ld.16y<-summarize(ag.fips,all.cases=sum(cases))
#summarizes lyme disease cases across all years 
ld.16y<-left_join(select(ld.prism.pop,c(state,county,fips)),ld.16y)
#this joins ld.prism.pop to the ld data by fips code and shows state, county and 
#fips codes as well as all cases
ld.16y<-distinct(ld.16y)
#Potentially got rid of duplicate rows/data
ld.16y %<>% rename(region=state,subregion=county)
#renamed state-> region, and county-> subregion
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")
#removed the word "County" from within the column subregion
ld.16y$region<-tolower(ld.16y$region)
#made the states all lowercase
ld.16y$subregion<-tolower(ld.16y$subregion)
#made all the counies/ subreagions lowercase
ld.16y %<>% mutate(log10cases=log10(1+all.cases))
#added a column of the log of all cases per county
map.ld.16y<-left_join(county_map,ld.16y)
#joined the county_map with ld.16y table
ggplot(map.ld.16y)+geom_point(aes(long,lat,color=log10cases),size=0.1) +
  scale_colour_gradientn(colours=rev(rainbow(4)))
#visual of the map.ld.16y
#plot shows lattitude and longitude and shows the subcounties (counties) with the 
#highest prevelance of lyme disease











