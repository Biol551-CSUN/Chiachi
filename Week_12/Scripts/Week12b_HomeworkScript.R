# Week 12b Homework 

# make a plot where one of the axis is a factor, save everything in the appropriate folders
# if you decide to visualize anything by tide height it should go from low to mid to high tide
# have to use intertidal.csv
# percent cover of some spceies 

# first clean up the names of the columns 

library(here)
library(tidyverse)
library(janitor)
install.packages("PNWColors")
library(PNWColors)

InteridalData<-read_csv(here("Week_12", "Data", "intertidaldata.csv")) 
view(InteridalData)

pal=pnw_palette("Shuksan2",3)
# we want to make a plot with tide height on the x axis (low, medium high)

# first remove the white space 
Intertidalclean<-InteridalData%>%
  clean_names()%>%
  mutate(quadrat = str_replace_all(quadrat, pattern = "\\.", replacement = ""))%>% # replace periods with nothing
  mutate(quadrat = str_replace_all(quadrat, pattern = "[:digit:]", replacement = ""))%>% # remove all the things we don't want
  mutate(quadrat = str_trim(quadrat, side = "right")) # trim the white space

view(Intertidalclean)
unique(Intertidalclean$quadrat) # make sure that our three values are what we want 
level_order <- c('Low', 'Mid', 'High') 
Intertidalclean%>%
  ggplot(aes(x = factor(quadrat, level = level_order), y = gooseneck_barnacles, fill = quadrat)) + 
  geom_col(show.legend = FALSE)+
  facet_wrap(~site, ncol = 3)+
  labs(x = "Tide Height", 
      y = "Number of Gooseneck Barnacles", 
      title = "Comparing the number of Gooseneck Barnacles at various California sites based on Tide Height")+
  scale_fill_manual(values = pal)+
  theme_classic()+
  ggsave(here("Week_12", "Output", "GooseneckSites.png"), width = 9, height = 9)
