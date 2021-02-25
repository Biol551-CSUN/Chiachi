####Week 5b Lecture Notes: lubridate ####
####Created by Amanda Chiachi####
####Last edited 02-24-2021####

###Load Libraries####
library(tidyverse)
library(devtools)
library(here)
library(lubridate)
library(PNWColors)

# what time is it now
now()
# what time is it on the east coast?
now(tzone="EST")
#if you only want the date, not the time
today() 
today(tzone = "GMT")
# you can also ask if it is morning or night
am(now())
# is it a leap year?
leap_year(now())

# first, your date must be a character
# use the glimpse function first, if the column isnt a character, change it to character
# mutate(as.character())
# 2021-02-24 is ymd()
# 02/24/2020 is dmy()

ymd("2021-02-24")
mdy("02/24/2021")
mdy("February 24 2021")
dmy("24/02/2021")

ymd_hms("2021-02-24 10:22:20 PM") # try to get in the habit of using military time
mdy_hms("02/14/2021 22:22:20")
mdy_hm("February 24 2021 10:22 PM")
 # all of these give you the exact same output 

# make the character string
datetimes<-c("02/24/2021 22:22:20", 
             "02/25/2021 11:21:20", 
             "02/26/2021 8:01:52")
# convert to datetimes
datetimes<-mdy_hms(datetimes)
month(datetimes)
month(datetimes, label = TRUE, abbr = FALSE)
day(datetimes) #extract day
wday(datetimes, label=TRUE) #extract day of the week 

datetimes + hours(4) # this adds 4 hours to all of the dates
datetimes + days (2) # add 2 days to all of the dates
# notice the s added to the end of hours

round_date(datetimes, "minute") # round to the nearest minute
round_date(datetimes, "5 mins") # round to the nearest 5 minutes

# read in the conductivity data
# convert the date column to a datetime 
# use %>% to keep everything clean

CondData<-read.csv(here("Week_5", "Data", "CondData.csv"))

CondData_date <- CondData %>%
  mutate(DateTime = ymd_hms(date))

# Today's R package
install_github("Gibbsdavidl/CatterPlots")
library(CatterPlots)
x <- c(1:10)
y <- x(1:10)
catplot(xs = x, ys = y, cat = 5, catcolor = 'pink')
# not sure why this isn't running
