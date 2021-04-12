### Lecture 10a Lab assignment
# Last Edited April 5, 2021

#Install Packages
install.packages('reprex') #reproducible example
install.packages('datapasta') #copy and paste
install.packages('styler') # copy and paste in style

#Load Libraries 
library(tidyverse)
library(here)

meteorites<- readr::read_csv()

tibble::tribble(
            ~County, ~Population,
      "los angeles",   10081570L,
        "san diego",    3316073L,
           "orange",    3168044L,
        "riverside",    2411439L,
   "san bernardino",    2149031L,
      "santa clara",    1927470L,
          "alameda",    1656754L,
       "sacramento",    1524553L,
     "contra costa",    1142251L,
           "fresno",     984521L,
             "kern",     887641L,
    "san francisco",     874961L,
          "ventura",     847263L,
        "san mateo",     767423L,
      "san joaquin",     742603L,
       "stanislaus",     543194L,
           "sonoma",     499772L,
           "tulare",     461898L,
    "santa barbara",     444829L,
           "solano",     441829L,
         "monterey",     433410L,
           "placer",     385512L,
  "san luis obispo",     282165L,
       "santa cruz",     273962L,
           "merced",     271382L,
            "marin",     259943L,
            "butte",     225817L,
             "yolo",     217352L,
        "el dorado",     188563L,
         "imperial",     180701L,
           "shasta",     179212L,
           "madera",     155433L,
            "kings",     150691L,
             "napa",     139623L,
         "humboldt",     135940L,
           "nevada",      99244L,
           "sutter",      96109L,
        "mendocino",      87224L,
             "yuba",      76360L,
             "lake",      64195L,
           "tehama",      63912L,
       "san benito",      60376L,
         "tuolumne",      54045L,
        "calaveras",      45514L,
         "siskiyou",      43468L,
           "amador",      38429L,
           "lassen",      30818L,
            "glenn",      27976L,
        "del norte",      27495L,
           "colusa",      21454L,
           "plumas",      18660L,
             "inyo",      17977L,
         "mariposa",      17420L,
             "mono",      14310L,
          "trinity",      12700L,
            "modoc",       8907L,
           "sierra",       3040L,
           "alpine",       1039L
  )

library(tidyverse)
library(here)
meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")
library(tidyverse)
library(here)
#> here() starts at C:/Users/kayey/AppData/Local/Temp/RtmpUtEZmj/reprex-27882bd74ba-keen-moa
meteorites <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-06-11/meteorites.csv")
#> 
#> -- Column specification --------------------------------------------------------
#> cols(
#>   name = col_character(),
#>   id = col_double(),
#>   name_type = col_character(),
#>   class = col_character(),
#>   mass = col_double(),
#>   fall = col_character(),
#>   year = col_double(),
#>   lat = col_double(),
#>   long = col_double(),
#>   geolocation = col_character()
#> )
world <- map_data("world")
meteorites_clean <- meteorites %>% 
  filter(year = "1990-2013")