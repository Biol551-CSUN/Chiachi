# Week 13 Lecture 
# April 26th 2021

library(tidyverse)
library(here)

# For Loops
# first add the basic iteration of the code
# we just want to print something, paste function to say the year is 2000
print(paste("The year is", 2000))
# "this comes out as This year is 2000

# first list out the sequence that you want to use
years<-c(2015:2021)
for (i in years){ # set up the for loop where i is the index
  print(paste("The year is", i)) # loop over i
}

# Make sure to Pre-allocate space for the for loop
# Empty matrix
year_data<-data.frame(matrix(ncol = 2, nrow = length(years)))
# add column names
colnames(year_data)<-c("year", "year_name")
year_data
# this just makes an empty data frame

# now we want to add in the for loop
for (i in 1:length(years)){ # set up the for loop where i is the index
  year_data$year_name[i]<-paste("The year is", years[i]) # loop over i
}
year_data

for (i in 1:length(years)){ # set up the for loop where i is the index
  year_data$year_name[i]<-paste("The year is", years[i]) # loop over year name
  year_data$year[i]<-years[i] # loop over year
}
year_data
# if you ever use python, julia, or matlab you are going to have to do this 

# point to the location on the computer of the folder

testdata<-read.csv(here("Week_13", "data", "cond_data","011521_CT316_1pcal.csv"))
glimpse(testdata) 

CondPath<-here("Week_13", "data", "cond_data")
# list all the files in that path with a specific pattern
# In this case we are looking for everything that has a .csv in the filename
# you can use regex to be more specific if you are looking for certain patterns in filenames
files <- dir(path = CondPath,pattern = ".csv")
files

# pre-allocate space
# make an empty dataframe that has one row for each file and 3 columns
cond_data<-data.frame(matrix(nrow = length(files), ncol = 3))
# give the dataframe column names
colnames(cond_data)<-c("filename","mean_temp", "mean_sal")
cond_data

raw_data<-read.csv(paste0(CondPath,"/",files[1])) # test by reading in the first file and see if it works
head(raw_data)

mean_temp<-mean(raw_data$Temperature, na.rm = TRUE) # calculate a mean
mean_temp

# Example 1
for (i in 1:length(files)){ # loop over 1:3 the number of files
}
for (i in 1:length(files)){ # loop over 1:3 the number of files 
  raw_data<-read.csv(paste0(CondPath,"/",files[i]))
  glimpse(raw_data)
}
# this should run three times
# now we add in the columns 
for (i in 1:length(files)){ # loop over 1:3 the number of files 
  raw_data<-read.csv(paste0(CondPath,"/",files[i]))
  #glimpse(raw_data) this is commented out because you dont want it popping up each time
  cond_data$filename[i]<-files[i]
} 
cond_data
# add in the means
for (i in 1:length(files)){ # loop over 1:3 the number of files 
  raw_data<-read.csv(paste0(CondPath,"/",files[i]))
  #glimpse(raw_data)
  cond_data$filename[i]<-files[i]
  cond_data$mean_temp[i]<-mean(raw_data$Temperature, na.rm =TRUE)
  cond_data$mean_sal[i]<-mean(raw_data$Salinity, na.rm =TRUE)
} 
cond_data
# now we have a nice clean dataframe with summary statistics for each file

## Mapping Functions #### 
# this is to pile and add things together

