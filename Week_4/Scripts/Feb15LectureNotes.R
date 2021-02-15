####Week 4a Lecture Notes: Data Wrangling: dplyr####
####Created by Amanda Chiachi####
####Last edited 02-15-2021####

#####Lecture Notes####
# extract rowas with filter()
# extract columns with select()
# make new columns with mutate()

###Load Libraries####
library(tidyverse)
library(here)
library(palmerpenguins)

#####Load the Data####
#The data is part of a package called penguins
glimpse(penguins)

# use filter to extract outlier (within a certain amount)
# filter out only the females 
# if you are using a number, you dont use quotes
# make sure to use two equal signs
filter(.data = penguins, 
       sex=="female")
glimpse(penguins) # this is still the original data, we did not save the filter

####logical expressions####
# x is the column that you are trying to filter from 
# x<==y less than or equal to
# x>=y greater than or equal to
# != y not equal to 
# x %in% y in(group membership)
# is.na(x) is missing
# !is.na(x) is not missing 

###Practice filter function####
#Penguins measured in the year 2008
filter(.data = penguins, 
       year == 2008)
#Penguins have a body mass greater than 5000
filter(.data = penguins,
       body_mass_g>5000)

# filter with multiple conditions
filter(.data = penguins, sex == "female", body_mass_g >4000)

#Penguins that were collected in either 2008 or 2009 
filter(.data = penguins,
       year == 2008 | year == 2009)
#Penguins that are not from the island Dream
#Need to check in on that 
filter(.data = penguins,
       island != "Dream")
#Penguins in the species Adelie and Gentoo
filter(.data = penguins, 
       species == "Adelie & Gentoo")

###Practice Mutate Function####
data2<-mutate(.data = penguins, 
              body_mass_kg = body_mass_g/1000)
view(data2)

data2<-mutate(.data = penguins, 
              body_mass_kg = body_mass_g/1000, 
              bill_length_depth = bill_length_mm/bill_depth_mm)
view(data2)

data2<-mutate(.data = penguins, 
              after_2008 = ifelse(year>2008, "After 2008", "Before 2008"))
view(data2)

#Use mutate to create a new column to add flipper length and body mass together
data2<-mutate(.data = penguins, 
              flipper_and_mass = flipper_length_mm + body_mass_g)

view(data2)
#Use mutate and ifelse to create a new column where male and female are capitalized
data2<-mutate(.data = penguins, 
              sex_cap = ifelse(sex == "male", "Male", "Female"))
view(data2)

#The Pipe####
# this means "and then do..."

# filter only female penguins nad add a new column that calculates the log body mass 
penguins %>%
  filter(sex == "female")%>%
  mutate(log_mass = log(body_mass_g))%>%
  view()

# select function
penguins %>%
  filter(sex == "female")%>%
  mutate(log_mass = log(body_mass_g))%>%
  select(species, island, sex, log_mass)%>%
  view()

# rename species to have a capital S
penguins %>%
  filter(sex == "female")%>%
  mutate(log_mass = log(body_mass_g))%>%
  select(Species = species, island, sex, log_mass)

# Summarise function####
#calculate the mean flipper length (and exclude and NAs)
penguins%>% 
  summarise(mean_flipper = mean(flipper_length_mm, na.rm=TRUE), 
            min_flipper = min(flipper_length_mm, na.rm = TRUE))

#use the group by function
penguins %>%
  group_by(island) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = max(bill_length_mm, na.rm = TRUE))
            
penguins %>%
  group_by(island, sex, species) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = max(bill_length_mm, na.rm = TRUE))

#drop na function 
penguins %>%
  drop_na(sex) %>%
  group_by(island, sex) %>%
  summarise (mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

#Plots#
# plots have a + not a pipe 
penguins %>%
  drop_na(sex) %>%
  ggplot(aes(x = sex, y = flipper_length_mm))+
  geom_boxplot()

#the dad joke package

