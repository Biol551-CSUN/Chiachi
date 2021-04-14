library(tidytuesdayR)
library(tidyverse)
library(here)
install.packages('plotly')
library(plotly)


# clear environment
rm(list = ls())
# load Tidy Tuesday data
tuesdata <- tidytuesdayR::tt_load('2020-02-18')

# assign data to dataframe
food.data<-tuesdata$food_consumption
glimpse(food.data)

fig <- plot_ly(data = food.data, 
               x = ~co2_emmission, 
               y = ~consumption)
fig

fig <- plot_ly(data = food.data, 
               x = ~co2_emmission, 
               y = ~consumption,
               color = ~food_category)
fig

# change the color pallette 
fig <- plot_ly(data = food.data, 
               x = ~co2_emmission, 
               y = ~consumption,
               color = ~food_category,
               colors = "viridis")
fig

# group by a continuous variable
fig <- plot_ly(data = food.data, 
               x = ~co2_emmission, 
               y = ~consumption,
               color = ~co2_emmission,
               colors = "viridis")
fig

# formatting customizations
fig <- plot_ly(data = food.data, 
               x = ~co2_emmission, 
               y = ~consumption,
               color = ~food_category,
               colors = "viridis",
               marker = list(size = 10), #change the marker size
               text = ~paste("CO2 emission:", co2_emmission, #change the hover data labels
                             "<br>Consumption:", consumption, #<br> moves the label to a new line
                             "<br>Food category:", food_category)) %>% #must PIPE to the layout 
  layout(title = 'CO2 Emissions and Food Consumption by Food Type', #add a title
         xaxis = list(title = "CO2 Emission"), #change the axes titles
         yaxis = list(title = "Consumption"),
         legend = list(title = list(text = 'Food Category'))) #add a legend title
fig

#turn your scatterplot into a line graph
fig <- plot_ly(data = food.data, 
               x = ~co2_emmission, 
               y = ~consumption,
               color = ~food_category,
               colors = "viridis",
               mode = 'lines', #change to lines
               text = ~paste("CO2 emission:", co2_emmission, 
                             "<br>Consumption:", consumption, 
                             "<br>Food category:", food_category)) %>% 
  layout(title = 'CO2 Emissions and Food Consumption by Food Type', 
         xaxis = list(title = "CO2 Emission"), 
         yaxis = list(title = "Consumption"),
         legend = list(title = list(text = 'Food Category'))) 

fig

#Basic Bar charts 
bar <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China", #select countries
         food_category == "Beef") #select 1 food category
bar <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China",
         food_category == "Beef") %>%
  plot_ly(x = ~country, #create bar chart showing beef consumption by country
          y = ~consumption,
          type = "bar")

bar
# create a stacked bar chart with custom hover labels
bar_stacked <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China") #filter out 3 countries of interest
#make two new columns
bar_stacked <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China") %>% 
  group_by(country) %>% 
  mutate(total_consumption = (sum(consumption)),
         percent_consumption = ((consumption/total_consumption)*100)) #calculate amount of each food consumed as percent of total consumption
bar_stacked <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China") %>% 
  group_by(country) %>% 
  mutate(total_consumption = (sum(consumption)),
         percent_consumption = ((consumption/total_consumption)*100)) %>%
  plot_ly(x = ~country, #countries on x-axis
          y = ~percent_consumption, #percent consumption on y-axis
          color = ~food_category, #color by type of food
          type = "bar") #make it a bar chart

bar_stacked

bar_stacked <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China") %>% 
  group_by(country) %>% 
  mutate(total_consumption = (sum(consumption)),
         percent_consumption = ((consumption/total_consumption)*100)) %>%
  plot_ly(x = ~country,
          y = ~percent_consumption,
          color = ~food_category,
          text = ~paste("Total consumption:", total_consumption, "kg/person/year"), #customize hover label text
          type = "bar")

bar_stacked

# to make it a stacked bar chart 
bar_stacked <- food.data %>%
  filter(country == "USA" | country == "United Kingdom" | country == "China") %>% 
  group_by(country) %>% 
  mutate(total_consumption = (sum(consumption)),
         percent_consumption = ((consumption/total_consumption)*100)) %>%
  plot_ly(x = ~country,
          y = ~percent_consumption,
          color = ~food_category,
          text = ~paste("Total consumption:", total_consumption, "kg/person/year"),
          type = "bar") %>% 
  layout(barmode = "stack", #stack bars
         title = "Percent of food comsumed by country", #change plot title
         xaxis = list(title = "Country"), #change x-axis title
         yaxis = list(title = "Percent of total consumption")) #change y-axis title

bar_stacked

to add a drop down menu
dropdown <- food.data %>%
  filter(country == "USA") %>% #filter out USA
  plot_ly(x = ~food_category) #plot with food category on x-axis

dropdown
# use the add_bars function
dropdown <- food.data %>%
  filter(country == "USA") %>%
  plot_ly(x = ~food_category) %>%
  add_bars(y = ~consumption, name = "Food Consumption") #create bar graph

dropdown

dropdown <- food.data %>%
  filter(country == "USA") %>%
  plot_ly(x = ~food_category) %>%
  add_bars(y = ~consumption, name = "Food Consumption") %>%
  add_bars(y = ~co2_emmission, name = "CO2 Emissions", visible = FALSE) #create second bar graph, make invisible

dropdown

dropdown <- food.data %>%
  filter(country == "USA") %>%
  plot_ly(x = ~food_category) %>%
  add_bars(y = ~consumption, name = "Food Consumption") %>%
  add_bars(y = ~co2_emmission, name = "CO2 Emissions", visible = FALSE) %>%
  layout(updatemenus = list(list(y = 0.6, #set vertical position of menu
                                 x = -0.2, #set horizontal position of menu
                                 buttons = list(list(method = "restyle", #use "buttons" to add the 2 different menu options
                                                     args = list("visible", list(TRUE, FALSE)), #show first plot, hide second plot
                                                     label = "Consumption"),
                                                list(method = "restyle",
                                                     args = list("visible", list(FALSE, TRUE)), #hide first plot, show second plot
                                                     label = "CO2 Emissions")))))

dropdown
