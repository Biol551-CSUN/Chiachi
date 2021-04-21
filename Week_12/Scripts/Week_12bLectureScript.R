library(tidyverse)
library(here)

tuesdata <- tidytuesdayR::tt_load(2021, week = 7)
income_mean<-tuesdata$income_mean
rm(tuesdata) #remove the tuesdata because it is so big and we don't need it

# to turn somethinginto a factor
fruits<-factor(c("Apple", "Grape", "Banana"))
fruits

# this can be scary! Why?
test<-c("A", "1", "2")
as.numeric(test) # you get a warning, and this is okay

# this is where things go wrong
test<-factor(test) # covert to factor
as.numeric(test)
# this can mess you up! know the difference and dont use factor unless you need to

###Read in the data safely####
# read.csv() strings are always read in as factors
# read_csv() strings will be read as characters  -- this is what we want! 

# lets practice by using forcats
glimpse(starwars)

starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE)
# these are read in as a character already 

star_counts<-starwars %>%
  filter(!is.na(species)) %>% 
  mutate(species = fct_lump(species, n = 3)) %>% # only show me the unique values that have more than three individuals
  count(species)
star_counts # this is now in alphabetical order 

# what if we want to show this 
star_counts %>%
  ggplot(aes(x = species, y = n))+
  geom_col()

# to view in order of lowest to highest
star_counts %>%
  ggplot(aes(x = fct_reorder(species, n), y = n))+ # reorder the factor of species by n
  geom_col()
# here you would add +labs to change the x label
# to go from highest to lowest add .desc = TRUE
star_counts %>%
  ggplot(aes(x = fct_reorder(species, n, .desc = TRUE), y = n))+ # reorder the factor of species by n
  geom_col() +
  labs(x = "Species")

# We will make a plot of the total income by year and quantile across all dollar types
total_income<-income_mean %>%
  group_by(year, income_quintile)%>%
  summarise(income_dollars_sum = sum(income_dollars))%>%
  mutate(income_quintile = factor(income_quintile)) # make it a factor

# make a basic line plot
total_income%>%
  ggplot(aes(x = year, y = income_dollars_sum, color = income_quintile))+
  geom_line()
# we can reorder line plots by using fct_reorder2, 
total_income%>%
  ggplot(aes(x = year, y = income_dollars_sum, 
             color = fct_reorder2(income_quintile,year,income_dollars_sum)))+
  geom_line()+
  labs(color = "income quantile")

x1 <- factor(c("Jan", "Mar", "Apr", "Dec"))
x1

x1 <- factor(c("Jan", "Mar", "Apr", "Dec"), levels = c("Jan", "Mar", "Apr", "Dec"))
x1

starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% # make species a factor
  filter(n>3) # only keep species that have more than 3
starwars_clean

levels(starwars_clean$species)

starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% # make species a factor 
  filter(n>3)  %>% # only keep species that have more than 3 
  droplevels() # drop extra levels
levels(starwars_clean$species)

# if you want to reame or recode your level names
starwars_clean<-starwars %>% 
  filter(!is.na(species)) %>% # remove the NAs
  count(species, sort = TRUE) %>%
  mutate(species = factor(species)) %>% # make species a factor 
  filter(n>3)  %>% # only keep species that have more than 3 
  droplevels() %>% # drop extra levels 
  mutate(species = fct_recode(species, "Humanoid" = "Human"))
starwars_clean
