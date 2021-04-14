
devtools::install_github("easystats/see")

install.packages("correlation")
install.packages("ggraph")
install.packages("gridExtra")
install.packages("performance")

library(tidyverse)
library(see)
library(here)
library(correlation)
library(ggraph)
library(tidytuesdayR)
library(gridExtra)
library(performance)

tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')

#view(tuition_cost) # check data structure, only used during early code composition
kirill_density <- tuition_cost # pass data to a new frame to preserve the original
x <- kirill_density %>% 
  pull(in_state_total) # create vector "x" with the data to be analyzed

density_kernel <- bayestestR::estimate_density(x) # perform the analysis, storing results in a new dataframe "density_kernel"

hist(x, prob = TRUE, main = NULL, width = 10, height = 7, noRStudioGD = TRUE, xlab = "Total in-state spendings") # create a histogram for demonstration of the results of the function, remove default title, set size and use a parameter for correct rendering, set x axis label
lines(density_kernel$x, density_kernel$y, col = "black", lwd = 2) # set line color and width
title("estimate_density function over traditional histogram", lwd = 1)
legend("topright",
       legend = ("estimate_density output"),
       col = ("black"), lwd = 2, lty = c(1, 2)  # add legend, set width and line type to match rendered line
)
