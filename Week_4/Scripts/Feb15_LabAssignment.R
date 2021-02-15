####Week 4a Homework####
####Created by Amanda Chiachi####
####Last edited 02-15-2021####

###Load Libraries####
library(tidyverse)
library(here)
library(palmerpenguins)
library(PNWColors)

#Write a script that: 
# 1. calculates the mean and variance of body mass by species, 
# island, and sex without any NAs
# (its own data frame)

pal <- pnw_palette("Sunset", 3, type = "discrete")

Mean_Var_Penguins<-
  penguins %>%
  drop_na(species, island, sex) %>% # drop all NAs in data for species, island and sex
  group_by(species, island, sex) %>% # group by species, island and sex
  summarise(mean_body_mass = mean(body_mass_g), # make new columns for mean body mass and variance 
            variance_body_mass = var(body_mass_g))

# 2. filters out (i.e. excludes) male penguins, then calculates 
# the log body mass, then selects only the columns for species, 
# island, sex, and log body mass, then use these data to make any plot.
# make sure the plot has clean and clear labels and follows best practices. 
# save the plot in the correct output folder. 

Male_Bodymass<-
  penguins %>%
  filter(sex != "male") %>% #filter out everything but males 
  group_by(species, island, sex)%>% #group by species, island and sex
  summarise(log_body_mass = log(body_mass_g))%>% #make a new column for log body mass 
  ggplot(aes(x = species, # x axis is the species of penguin
             y = log_body_mass, # y axis is the body mass of the penguins
             color = species))+ # each species has a different color 
  geom_boxplot(show.legend = FALSE)+ # remove the legens on the right (repetetive)
 # facet_wrap(~island)+ # can facet for islands if you want to look at body mass of the species per island 
  labs(title = "Body Mass of Three Species of Male Penguins", # add the top label 
       x = "", # remove the x label (repetetive)
       y = "Body Mass (g)")+ # re-label y label 
  theme_light()+ # remove the background color and make the plot look a bit simpler
  theme(axis.title = element_text(size = 15))+ #change the size of the axis 
  scale_color_manual(values = pal)+ #add the colors from the saved "pal"
  ggsave(here("Week_4", "Output", "MaleMass_Species.png"), #save the plot!
         width = 7 , height = 5)

  

