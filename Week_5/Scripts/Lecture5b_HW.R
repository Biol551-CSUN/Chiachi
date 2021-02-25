####Week 5b Group Work and Homework: lubridate ####
####Created by Amanda Chiachi####
####Last edited 02-24-2021####

###Load Libraries####
library(tidyverse)
library(devtools)
library(here)
library(lubridate)
library(PNWColors)
library(dplyr)

### Read in Conductivity and Depth Data #### 
CondData<-read_csv(here("Week_5", "Data", "CondData.csv"))
DepthData<-read_csv(here("Week_5", "Data", "DepthData.csv"))

### Data analysis ####

# convert dates columns appropriately
CondData_date <- CondData %>% # name new data frame
  mutate(DateTime = ymd_hms(date)) %>% # indicate the current date orientation 
  mutate(DateTime + seconds(8))%>% # add 8 seconds to each time point to make it an even 10 minutes
  rename(date_old = date) %>% # rename the old column (we don't want to use it)
  rename(na_date = DateTime)%>% # rename the old column (we don't want to use it)
  rename(date = "DateTime + seconds(8)")%>% # rename the new column (we don't want to use it)
  select(-date_old, -na_date) # removes unnecessary columns 

DepthData_date <- DepthData %>% # new data frame
  mutate(DateTime = ymd_hms(date)) # indicate the current date orientation

FullData<- inner_join(CondData_date, DepthData_date) # join the two dataframes 

FullData <- FullData%>% # name data frame to see changes as you go 
  mutate(time_hour = hour(date))%>% # pull out hour and make it a new column 
  mutate(time_minute = minute(date))%>% # pull out minute and make it a new column 
  group_by(time_hour, time_minute) %>% # keep time_hour and time_minute
  summarise(mean_date = mean(date), # take average of date 
            mean_depth = mean(Depth), # take average depth 
            mean_temperature = mean(TempInSitu), # take average temp 
            mean_salinity = mean(SalinityInSitu_1pCal))%>% # take average salinity
write_csv(here("Week_5", "Output", "FullData.csv")) #export csv to the correct folder

### Plot Data ####
FullData %>% # name the data frame 
  ggplot(aes(x = mean_temperature, # name x axis 
             y = mean_depth, # name y axis 
             color = mean_salinity))+ # make the color spectrum the salinity
           geom_line()+ # line graph
  labs(title = "Average Temperature, Depth and Salinity", # Label the title
       x = "Average Temperature (Degrees C)", # Label the x axis 
       y = "Average Depth (Meters)")+ # Label the y axis 
  theme_light()+ # change theme, remove background color etc.
  theme(axis.title = element_text(size = 15)) # change axis title text size  
  ggsave(here("Week_5", "Output", "AvgTempDepthSal.png"), #save the plot!
         width = 7 , height = 5) 
