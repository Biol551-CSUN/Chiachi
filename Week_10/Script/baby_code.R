# Shiny App 

library(tidyverse)
library(kableExtra)
library(shiny)
library(here)
library(lubridate)

# Load in our data 
BabyData<-read_csv(here("Week_10", "Data", "HatchBabyExport.csv")) 

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Baby Weight"),
  
  # Sidebar with a dropdown menu for breed
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "Baby Name",
        label = "Baby Name:",
        choices = unique(BabyWeight$`Baby Name`),
        selected = "Blakely" # default selection
      ), 
    ),
    
    # Show a plot of the city-wide distribution
    mainPanel(column(6, # column() modifies the layout (# is the column width)
        h4("Baby Weight over time"),
        plotOutput("distPlot")),
      p(), # a line break
      p(),
      )
    ) # /mainPanel
  ) # /fluidPage


# Server logic
server <- function(input, output) {
  output$distPlot <- renderPlot({
    # count of chosen breed x by district
    BabyWeight %>%
      filter(`Baby Name`== input$`Baby Name` & !is.na(Amount)) %>%
      ggplot(aes(x = factor(`Baby Name`))) +
      xlab("Date") +
      ylab("Amount of Formula (g)") +
      ggtitle(paste("Count of", input$BabyName, "\n in each district", sep = " ")) +
      geom_bar(fill = "#74CEB7") +
      theme_classic(base_size = 16)
  })
  
  # use inputs here to subset the data to the user's district of choice:
  # breed = "Shih Tzu"
  
  output$birthdayPlot <- renderPlot({
    dogs %>%
      filter(BREED == input$breed & !is.na(DISTRICT)) %>%
      filter(DOG_BIRTHDAY < 2020) %>%
      ggplot(aes(
        x = factor(DISTRICT),
        y = DOG_BIRTHDAY
      )) +
      geom_boxplot() +
      xlab("District") +
      ylab("Dog's birth year") +
      theme_classic(base_size = 16)
  })
}

# Run the application
shinyApp(ui = ui, server = server)

BabyWeight <- BabyData %>%
  filter(Activity == "Weight")%>%
  separate(col = `Start Time`, 
           into = c("Date", "Time"), 
           sep = " ")%>%
  separate(col = Date, 
           into = c("Day", "Month", "Year"), 
           sep = "/")%>%
  mutate(BabyWeight, Date = paste(Year, Month, Day, sep = "-"))%>%
  select(-Time, -'End Time', -Percentile, -Duration, -Info, -Notes, -X10, -Day, -Month, -Year)


view(BabyWeight)

BabyWeight%>%
  ggplot(aes(x = Date, y = Amount))+
  geom_point()

#group_by(month,day) -- sum up the total number of ounces 
#weight is in ounces, weight is in pounds 

view(BabyWeight)

BabyFeeding <- BabyData %>%
  filter(Activity == "Feeding")%>%
  separate(col = `Start Time`, 
           into = c("Date", "Time"), 
           sep = " ")%>%
  separate(col = Date, 
           into = c("Day", "Month", "Year"), 
           sep = "/")%>%
  select(-Time, -'End Time', -Percentile, -Duration, -Notes, -X10, -Year)



view(BabyFeeding)

