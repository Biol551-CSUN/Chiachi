####Week 5a Lecture Notes: Data wrangling:joins ####
#Today we are going to practice join with data from Becker and Silbiger####
####Created by Amanda Chiachi####
####Last edited 02-22-2021####

###Load Libraries####
library(tidyverse)
library(here)
library(PNWColors)

#What function do use to split up one column into multiple columns
#Separate
#What function to transition from wide to long 
#pivot longer

#When you collect data, make sure you have a unique identifier for site
#this will be helpful when you need to connect multiple data sets together 

###Load Data####
#Environmental Data from each site
EnviroData<-read.csv(here("Week_5", "Data", "site.characteristics.data.csv"))

#Thermal performance data
TPCData<-read_csv(here("Week_5", "Data", "Topt_data.csv"))

glimpse(EnviroData)
glimpse(TPCData)

EnviroData_wide<- EnviroData %>%
  pivot_wider(names_from = parameter.measured,  #pivot the data wider
              values_from = values) %>%
  arrange(site.letter) #arrange the dataframe by site
View(EnviroData_wide)

#left_join, they bring the two data frames together into one single data frame 
#needs to be the unique key between the two data frames 

FullData_left<- left_join(TPCData, EnviroData_wide) %>%
  relocate(where(is.numeric), .after = where(is.character))
##Joining, by = "site.letter"
head(FullData_left)

#take this data set and calculate the mean and variance of all of the collected TPC and environmental data by site
# summarise_at() or pivot data longer 

FullData_left_long<-FullData_left %>%
  pivot_longer(cols = E:substrate.cover, 
               names_to = "Variables", 
               values_to = "Values") %>%
  group_by(Variables, site.letter) %>%
  summarise(Mean_value = mean(Values, na.rm = TRUE), 
            Variance_Value = var(Values, na.rm = TRUE))
view(FullData_left_long)

OR 
# fulldata_left%>%
# group_by(site.letter%>%)
# summarise_at(vars(E:substrate.cover), list(mean = mean, var = var), na.rm = TRUE)%>%

# Make a tibble
T1<- tibble(Site.ID = c("A", "B", "C", "D"),
            Temperature = c(14.1, 16.7, 15.3, 12.8))
T1

T2<- tibble(Site.ID = c("A", "B", "D", "E"), 
            ph = c(7.3, 7.8, 8.1, 7.9))
T2

# lets compare left join and right join
left_join(T1, T2)
right_join(T1, T2)

# inner_join (only keeps the rows that are present in both 
# full_join brings everything together 
inner_join(T1, T2)
full_join(T1, T2)
#semi_join - a way to get rid of data that doesnt have NAS
#anti_join - only thing you keep is the thing thats missing
semi_join(T1, T2)
anti_join(T1, T2)

install.packages("cowsay")
library(cowsay)
say("i am a fish", by = "fish")
?cowsay
say("I am the superior animal", by = "chicken")

#re-do the markdown lab from week 2
