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
# map() makes a list.
# map_lgl() makes a logical vector.
# map_int() makes an integer vector.
# map_dbl() makes a double vector.
# map_chr() makes a character vector.
# map_df() makes a dataframe

# There are three ways to do the same thing in map() function
# first create a vector
1:10 # a vector from 1 to 10 (we are going to do this 10 times)
# for each time 1:10 make a vector of 15 random numbers
1:10 %>% # a vector from 1 to 10 (we are going to do this 10 times) %>% # the vector to iterate over
  map(rnorm, n = 15) # calculate 15 random numbers based on a normal distribution in a list
# calculate a mean for each of these random set of numbers
1:10 %>% # a vector from 1 to 10 (we are going to do this 10 times) %>% # the vector to iterate over
  map(rnorm, n = 15)  %>% # calculate 15 random numbers based on a normal distribution in a list 
  map_dbl(mean) # calculate the mean. It is now a vector which is type "double"
1:10 %>% # list 1:10
  map(function(x) rnorm(15, x)) %>% # make your own function
  map_dbl(mean)
# use a formula when you want to change the arguments
1:10 %>%
  map(~ rnorm(15, .x)) %>% # changes the arguments inside the function
  map_dbl(mean)
# find the files
# point to the location on the computer of the folder
CondPath<-here("Week_13", "data", "cond_data")
files <- dir(path = CondPath,pattern = ".csv")
files
# or we can get the full file names by 
files <- dir(path = CondPath,pattern = ".csv", full.names = TRUE)
#save the entire path name
files
# next read in the files using map instead o a for loop
data<-files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(read_csv,.id = "filename") # map everything to a dataframe and put the id in a column called filename
data
# now we are just caluculating mean temp and salinity
data<-files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(read_csv,.id = "filename") %>% # map everything to a dataframe and put the id in a column called filename
  group_by(filename) %>%
  summarise(mean_temp = mean(Temperature, na.rm = TRUE),
            mean_sal = mean(Salinity,na.rm = TRUE))
data
