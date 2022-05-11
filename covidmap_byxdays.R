### covidmap_byxdays.R ###
### creates a US map of covid rates at the county level
### for each week since late january 2020

### amelia hoover green ###
### draft of january 2022 ###

### setup ###
### others will comment the following line out ###
setwd('~/Dropbox/COVID/')

### packages ###

library(BBmisc)
library(scales)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(lubridate)
library(mapproj)

### functions ###

fips2 <- function(fipscode){ # get 5-digit fips from (maybe) 4-digit fips
  if(is.na(fipscode)) return(NA)
  else{
    while(nchar(as.character(fipscode))<5) {
      fipscode <- paste('0',fipscode,sep='')
    }
    return(fipscode)
  }
}

make_thisweek_fig <- function(w){ # the main function, takes the week as input
  tmp <- filter(cnty,week==w) # filter to this week's county-days only
  dt <- unique(tmp$end_date) # save end date for the period
  cntyshapes <- left_join(cntyshapes,tmp) %>% # join this week's data to county shape data
    group_by(fips) # and group by fips code
  
  covidmap <- ggplot(data=cntyshapes) + 
    geom_polygon(aes(x=long,y=lat,group=group,fill=nc_correct),
                 # make a polygon for each county with fill = corrected daily cases/100k
                 # "corrected" in the sense that variation at the top needs to be truncated
                 # in order to see variation at the bottom
                 color='white',lwd=0.1) +
    scale_fill_viridis_c(option="magma",limits=c(0,250),
                         na.value='grey',direction=-1) +
    # viridis is colorblind-friendly (and also nice looking!)
    coord_map("bonne",parameters=35) + # choose map coordinates
    theme_void()+ # ditch everything that's not the map axis lines, ticks, etc.
    labs(x='',y='',fill="", # labels
         title=paste("Average (reported) new COVID cases/100k/day, past ",w," days, by county",sep=''),
         subtitle = paste("Week ending ", dt,sep='')) +
    theme(legend.position=c(0.9,0.5)) +
    geom_path(data=stshapes, # notice this is a different dataset that i'm pulling data from
              aes(x=long,y=lat,group=group),
              color='white',lwd=0.25)
  
  if(nchar(w)==1) w <- paste('0',w,sep='') # this is so that the files get saved in order; makes later animation easier

  ggsave(paste('covidmap_images/covidmap_week',w,'.png',sep=''),
         height=3.5,width=7,dpi=150,
         device='png')
  rm(tmp)
}

### date stuff ###

days <- as.numeric(ymd(today())-ymd('2020-01-21'))
weeks <- round(days/7)

### map data ###

# fips codes #
f <- tibble(get(data(county.fips))) 
# county.fips comes with the maps package

# shapefiles #
cntyshapes <- as_tibble(map_data('county')) %>% # make a tibble of this data
  mutate(polyname=paste(region,subregion,sep=',')) %>%
  filter(region %in% c('alaska','hawaii')==FALSE) %>% # get rid of AK, HI (sorry)
  left_join(.,f) # join county data to fips data

stshapes <- as_tibble(map_data('state')) %>% # same dance except w/states
  mutate(polyname=paste(region,subregion,sep=',')) %>%
  filter(region %in% c('alaska','hawaii')==FALSE) %>% # get rid of AK, HI (sorry)
  left_join(.,f)

# the next two lines are inefficient, but they work 
# haven't figured out a better way yet.
cntyshapes$fips <- unlist(lapply(cntyshapes$fips,fips2))
stshapes$fips <- unlist(lapply(stshapes$fips,fips2))

# covid data #
print('getting COVID data')
cnty2020 <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-counties-2020.csv')
cnty2021 <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-counties-2021.csv')
cnty2022 <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-counties-2022.csv')

cnty <- bind_rows(cnty2020,cnty2021,cnty2022) %>%
  mutate(fips=substr(geoid,5,nchar(geoid)))

cnty$fips[cnty$county=='New York City'] <- 36061
  # for whatever reason the fips code in the Times data doesn't match the shapefile fips code

cnty <- 
  mutate(cnty,
         day=cut_interval(ymd(date),days,labels=c(1:days)), # using the number of days and weeks we calcuated above,
         week=cut_interval(ymd(date),weeks,labels=c(1:weeks))) %>% # get the day and week we're on now.
  group_by(fips,week) %>% # group by county and week
  summarize(new_cases_per100k=max(cases_avg_per_100k,na.rm=TRUE), # use the highest rolling average from this county*week
            end_date=max(date,na.rm=TRUE)) %>% # use the final day of the week
  ungroup() %>%
  group_by(week) %>%
  mutate(pctile.01=quantile(new_cases_per100k,0.01,na.rm=TRUE),
         pctile.99=quantile(new_cases_per100k,0.99,na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(nc_correct=ifelse(new_cases_per100k>250,250,new_cases_per100k),
         nc_correct=ifelse(is.na(nc_correct),0,nc_correct)) 
  
for(i in 1:weeks) {
  print(paste("Making graph for period",i))
  make_thisweek_fig(i)
}
