#### Today we are going to plot penguin data for our homework! ####
### Created by: Amanda Chiachi ##
### Updated on 2021-02-10

### Load Libraries####
library(palmerpenguins)
library(tidyverse)
library(dplyr)
library(here)
library(PNWColors)

####Load data####
#The data is part of the package and is called penguins
glimpse(penguins)
head(penguins)
tail(penguins)

pal <- pnw_palette("Starfish", 3, type = "discrete")

ggplot(data = penguins, #bring in my penguin data
       mapping = aes(x=factor(island), #x axis is island
                     y = body_mass_g, #y axis is Body Mass (g)
                     color = island))+ #different colors are 
  geom_violin(show.legend = FALSE)+ #remove legend (repetitive)
  facet_wrap(~species)+ #wrap by species
  labs(title = "Body Mass of Different Penguin Species on Three Islands", #add your title
    x = "", 
       y = "Body Mass (g)")+
  theme_light()+ #make the background lighter
  theme(axis.title = element_text(size = 15))+ #change the size of the axis 
  scale_y_continuous(limits = c(2500,6500))+ 
  scale_color_manual(values = pal)+ #add the colors from the saved "pal"
  ggsave(here("Week_3", "Output", "labpenguin_group5.png"), #save the plot!
  width = 7 , height = 5)


