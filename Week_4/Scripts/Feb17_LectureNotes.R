####Week 4b Lecture Notes: Data Wrangling: tidyr####
####Created by Amanda Chiachi####
####Last edited 02-17-2021####

#####Lecture Notes####


###Load Libraries####
library(tidyverse)
library(here)
library(ggbernie)

## Today's R Package - Bernie! ####
devtools::install_github("R-CoderDotCom/ggbernie@main")

ggplot(ChemData)+
  geom_bernie(aes(x = Salinity, y = NN, bernie = "sitting"))

#### Load Data ####
ChemData<-read_csv(here("Week_4","Data", "chemicaldata_maunalua.csv"))
View(ChemData)
glimpse(ChemData)

# Another way to remove all the NAs 
ChemData_clean<-ChemData %>%
  filter(complete.cases(.)) #filters out everything that is not a complete row 
view(ChemData_clean) # always view the data frames, esp when you have a bunch of pipes

# Separate Function ####
# Tide_time needs to be fixed 
?separate
# data = [data frame that you are using]
# col = [column that you want to separate]
# into = [name of the new columns]
# sep= [what are you separating by]

ChemData_clean<-ChemData %>%
  filter(complete.cases(.)) %>% #filters out everything that is not a complete row 
  separate(col = Tide_time, # choose the tide time column
           into = c("Tide", "Time"), # separate it into two columns Tide and Time 
           sep = "_" ) # separate by _ 
  # if you want to keep the original column
  # remove = FALSE in new line 
head(ChemData_clean)

# Unite function ####
ChemData_clean<-ChemData %>%
  filter(complete.cases(.)) %>% #filters out everything that is not a complete row 
  separate(col = Tide_time, # choose the tide time column
           into = c("Tide", "Time"), # separate it into two columns Tide and Time 
           sep = "_", 
           remove = FALSE) %>%
  unite(col = "Site_Zone", # the name of the NEW column
        c(Site,Zone), # the columns to unite
        sep = ".", # lets put a . in the middle
        remove = FALSE) # keep the original
head(ChemData_clean)

# Pivot data so that it is in long format ####
# can facet wrap by variable name ()
ChemData_long<-ChemData_clean %>%
  pivot_longer(cols = Temp_in:percent_sgd, #take the colums from temperature to percent sgd (if not in order, c(Temp_in, .....
               names_to = "Variables", # the names of the new columbs with all the column names
               values_to = "Values") # the names of the new column with all the values 
view(ChemData_long)

# Calculate the mean and variance for all variables at each site 
ChemData_long %>%
  group_by(Variables,Site) %>%
  summarise(Param_means = mean(Values, na.rm = TRUE), 
            Param_vars = var(Values, na.rm = TRUE))

# Calculate mean, variance, and standard deviation for all variables by site, zone and tide 

ChemData_long %>%
  group_by(Variables, Site, Zone, Tide) %>%
  summarise(Param_means = mean(Values, na.rm = TRUE), 
            Param_vars = var(Values, na.rm = TRUE), 
            Param_sd = sd(Values, na.rm = TRUE))

# Facet_wrap for plotting long data 
ChemData_long %>%
  ggplot(aes(x = Site, y = Values))+
  geom_boxplot()+ 
  facet_wrap(~Variables, scales = "free") #this makes the scale more accurate

# Converting long data back go wide####
ChemData_wide<-ChemData_long %>%
  pivot_wider(names_from = Variables, # column with the names for the new columns 
              values_from = Values) # column with the values
view(ChemData_wide)

##Calculate some summary statistics and export the csv file ###
ChemData_clean<-ChemData %>%
  filter(complete.cases(.)) %>%
  separate(col = Tide_time, 
           into = c("Tide", "Time"), 
           sep = "_", 
           remove = FALSE) %>%
  pivot_longer(cols = Temp_in:percent_sgd, 
               names_to = "Variables", 
               values_to = "Values") %>%
  group_by(Variables, Site, Time) %>% #this converts to long format 
  summarise(mean_vals = mean(Values, na.rm = TRUE)) %>%
  pivot_wider(names_from = Variables,   # notice it is now mean_vals as the column name
              values_from = mean_vals) %>% 
  write_csv(here("Week_4", "Output", "summary.csv")) #export csv to the correct folder
view(ChemData_clean)

# you can publish this as html format that is publication quality 



