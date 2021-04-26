# Week 12b Homework 

# make a plot where one of the axis is a factor, save everything in the appropriate folders
# if you decide to visualize anything by tide height it should go from low to mid to high tide
# have to use intertidal.csv
# percent cover of some spceies 

# first clean up the names of the columns 
# load libraries
library(here)
library(tidyverse)
library(janitor)
install.packages("PNWColors")
library(PNWColors)

InteridalData<-read_csv(here("Week_12", "Data", "intertidaldata.csv"))  # bring in the data
view(InteridalData) # look at the data 

pal=pnw_palette("Shuksan2",3) # select the color pal I want to use
# we want to make a plot with tide height on the x axis (low, medium high)

# first remove the white space 
Intertidalclean<-InteridalData%>% 
  clean_names()%>% # clean up the names 
  mutate(quadrat = str_replace_all(quadrat, pattern = "\\.", replacement = ""))%>% # replace periods with nothing
  mutate(quadrat = str_replace_all(quadrat, pattern = "[:digit:]", replacement = ""))%>% # remove all the things we don't want
  mutate(quadrat = str_trim(quadrat, side = "right")) # trim the white space

view(Intertidalclean)
unique(Intertidalclean$quadrat) # make sure that our three values are what we want 
level_order <- c('Low', 'Mid', 'High') # determine what order we want our x axis values to be in 
Intertidalclean%>%
  ggplot(aes(x = factor(quadrat, level = level_order), y = gooseneck_barnacles, fill = quadrat)) + # indicate what values go on the x and y axis 
  geom_col(show.legend = FALSE)+ # make a column plot and remove the legend 
  facet_wrap(~site, ncol = 3)+ # facet wrap by site 
  labs(x = "Tide Height", # Label the x axis 
      y = "Number of Gooseneck Barnacles",# label the y axis 
      title = "Comparing the number of Gooseneck Barnacles at various California sites based on Tide Height")+ # add a title
  scale_fill_manual(values = pal)+ # change the color of the plot 
  theme_classic()+ # classit theme for plot 
  ggsave(here("Week_12", "Output", "GooseneckSites.png"), width = 9, height = 9) # save my plot!
