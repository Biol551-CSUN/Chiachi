## GGplot script from lecture
## Created by: Amanda Chiachi
## Created on: February 8rd 2021
##### 

###Load Libraries ####
library(tidyverse)
library(palmerpenguins)
lin

###Read In Data ####
glimpse(penguins)

###Data Analysis###
#start with the basic line, to stage your plot (this gets it ready) should be a plain grey box
#this is the default 
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm, 
                     color = species, 
                     shape = species, 
                     size = body_mass_g, 
                     alpha = flipper_length_mm)) +
      geom_point(size = 2, alpha = 0.5)+
        labs(title = "Bill depth and length", #add your title
                  subtitle = "Dimensons for Adelie, Chinstrap and Gentoo Penguins", #add your subtitle
                  x = "Bill depth (mm)", 
                  y = "Bill length (mm)", #add labels
                  color = "Species",
                  shape = "Species",
                  caption = "Source: Palmer Station LTER/palmerpenguins package")+
        scale_color_viridis_d()+
        theme_classic()

## another plot
ggplot(penguins, 
       aes( x = bill_depth_mm, 
            y = bill_length_mm))+
  geom_point()+
  facet_grid(species~sex)+ #can switch this by putting (sex~species)
  theme_bw()

# or use facet_wrap by species, and specify that you want it two columns 
ggplot(penguins, 
       aes( x = bill_depth_mm, 
            y = bill_length_mm))+
  geom_point()+
  facet_wrap(~species, ncol = 2)+ #can switch this by putting (sex~species)
  theme_bw()

#facet and color, no legend
ggplot(penguins, 
       aes( x = bill_depth_mm, 
            y = bill_length_mm, 
            color = species))+
  geom_point()+
  scale_color_viridis_d()+
  facet_grid(species~sex)+
  guides(color = FALSE)+ #this gets rid of our legend
  theme_bw()

