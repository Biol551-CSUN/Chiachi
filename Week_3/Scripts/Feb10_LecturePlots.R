#### Today we are going to plot penguin data ####
### Created by: Amanda Chiachi ##
### Updated on 2021-02-10

### Load Libraries####
library(palmerpenguins)
library(tidyverse)
library(here)
library(praise)
library(PNWColors)

####Load data####
#The data is part of the package and is called penguins
glimpse(penguins)
head(penguins)
tail(penguins)

###Data Analysis####

pal <- pnw_palette("Cascades", 3, type = "discrete")

ggplot(data=penguins, 
       mapping = aes(x=bill_depth_mm, 
                     y = bill_length_mm, 
                     group = species, #this means everything below this will be grouped by species
                     color = species))+ 
  geom_point()+
  geom_smooth(method = "lm")+ #plots best fit line (this makes it a linear model)
    labs(x = "Bill depth (mm)", 
         y = "Bill length (mm)")+
  #scale_color_viridis_d()
  #scale_color_manual(values = c("orange", "purple", "green"))
  scale_color_manual(values = pal)+
  #coord_fixed()
  #coord_flip()+ #flips axis 
  #coord_polar("x")+
  theme_bw()+
  theme(axis.title = element_text(size = 20, 
                                  color = "darkgreen"), 
        panel.background = element_rect(fill = "lightgrey"), 
        legend.position = "top")+ # makes labels text larger
  scale_x_continuous(breaks = c(14, 17, 21), 
                     labels = c("low", "medium", "high"))

ggsave(here("Week_3", "Output", "penguin.png"))
#scale_x_continuous(breaks, limits... = c(14, 17, 21))                 
#we are making a vector that goes from 0 to 20 (c means concatanate)

praise()


?geom_smooth
?scale_x_continuous

