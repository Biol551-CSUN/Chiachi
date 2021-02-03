## This is my first script. I am learning how to importa data in the appropriate folder
## Created by: Amanda Chiachi
## Created on: February 3rd 2021
##### 
###Load Libraries ####
library(tidyverse)
library(here)

###Read In Data ####
WeightData<-read_csv(here("Week_2", "Data", "weightdata.csv"))

###Data Analysis###
head(WeightData)
tail(WeightData)
view(WeightData
     