# Week 13 Homework Script
# code should run and end with data frame (both times)

#### The For Loop Way ####
# point to the location on the computer of the folder
TidePath<-here("Week_13", "Data", "homework")
# list all the files in that path with a specific pattern
# In this case we are looking for everything that has a .csv in the filename
# you can use regex to be more specific if you are looking for certain patterns in filenames
files <- dir(path = TidePath,pattern = ".csv")
files
# make an empty data frame
# pre-allocate space
tide_data<-data.frame(matrix(nrow = length(files), ncol = 5)) # this makes an empty data frame with 5 columns
colnames(tide_data)<-c("filename","mean_temp", "sd_temp", "mean_light", "sd_light") # give the dataframe column names
tide_data # this shows all nas 
# basic code to caluclate the mean and sd
raw_data<-read.csv(paste0(TidePath,"/",files[i])) # test by reading in the first file and see if it works
head(raw_data) # test to make sure it works 
mean_temp<-mean(raw_data$Temp.C, na.rm = TRUE) # calculate a mean
mean_temp # this works 
# turn it into a for loop
for (i in 1:length(files)){ # loop over 1:3 the number of files 
  raw_data<-read_csv(paste0(TidePath,"/",files[i]))
  #glimpse(raw_data)
  tide_data$filename[i]<-files[i] # pull each file 
  tide_data$mean_temp[i]<-mean(raw_data$Temp.C, na.rm =TRUE) # mean temperature
  tide_data$sd_temp[i]<-sd(raw_data$Temp.C, na.rm =TRUE) # sd temperature
  tide_data$mean_light[i]<-mean(raw_data$Intensity.lux, na.rm = TRUE) # mean light intensity 
  tide_data$sd_light[i]<-sd(raw_data$Intensity.lux, na.rm =TRUE) # sd light intensity 
} 
tide_data # this shows mean temp, sd_temp, mean_light, and sd_light

#####The Purr Way####
# bring in the files with purr instead of a for loop
# point to the location on the computer of the folder
files <- dir(path = TidePath,pattern = ".csv", full.names = TRUE) #save the entire path name
files
# read in the files using map instead of a for loop 
tide_data_purr<-files %>%
  set_names()%>% # set's the id of each list to the file name
  map_df(read_csv,.id = "filename")%>% # map everything to a dataframe and put the id in a column called filename
  group_by(filename) %>%
  summarise(mean_temp = mean(Temp.C, na.rm = TRUE), # to find the mean temp
            sd_temp = sd(Temp.C,na.rm = TRUE),# to find the sd of temp
            mean_light = mean(Intensity.lux, na.rm = TRUE), # to find the mean intensity 
            sd_light = sd(Intensity.lux, na.rm = TRUE)) # to find the sd of the intensity 
tide_data_purr
