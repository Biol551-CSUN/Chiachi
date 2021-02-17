####Week 4b Homework: Data Wrangling: tidyr####
####Created by Amanda Chiachi####
####Last edited 02-17-2021####

###Load Libraries####
library(tidyverse)
library(here)
library(ggbernie)
library(PNWColors)

#### Load Data ####
ChemData<-read_csv(here("Week_4","Data", "chemicaldata_maunalua.csv"))
View(ChemData)
glimpse(ChemData)

pal <- pnw_palette("Cascades", 3, type = "discrete")

####Analyze Data####
ChemData_clean<-ChemData %>% #create new dataset 
  filter(complete.cases(.)) %>% #remove all of the NAs
  separate(col = Tide_time, # separate the Tide_time column into appropriate columns for analysis 
           into = c("Tide", "Time"), #label the colums "Tide" and "Time"
           sep = "_", # separate by _
           remove = TRUE) %>% # remove the original file 
  pivot_longer(cols = Salinity:Silicate, # separate the data between salinity and silicates
               names_to = "Variables",  # combine this data into one column called variables
               values_to = "Values") %>% #  
  group_by(Variables, Site, Time, Tide) %>% # convert to long format 
  summarise(mean_vals = mean(Values, na.rm = TRUE),  #summarise the stats that you want to look at, make them each a new column 
            vars_vals = var(Values, na.rm = TRUE), #variance
            sd_vals = sd(Values, na.rm = TRUE)) %>% #standard deviation 
write_csv(here("Week_4", "Output", "summary_homework.csv")) #export csv to the correct folder

###Plot Data #####
ChemData_clean %>% #bring in data
  ggplot(aes(x = Variables, # x axis is the variables (phosphate, salinity, silicate)
             y = mean_vals,  # y axis is the mean_values
             color = Variables))+ # make each variable a different color 
    geom_count(show.legend = FALSE)+ #make a geompoint variant 
  facet_wrap(~Tide)+ # separate into two plots for high and low tide 
  labs(title = "Average Nutrient Levels in High vs. Low tide", # title your plot
       x = "", # remove the x axis label
       y = "Mean Nutrient Values (umol/L)")+ # re-name the y axis (used the data dictionary to find the units)
  theme_light()+ # change theme, remove background color etc.
  theme(axis.title = element_text(size = 15))+ # change axis title text size 
  scale_color_manual(values = pal)+ # use the pnw color pallete 
  ggsave(here("Week_4", "Output", "Nutrients_highvlow.png"), #save the plot!
         width = 7 , height = 5) 

