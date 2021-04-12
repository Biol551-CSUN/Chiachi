#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Shiny overview
# Example 1
# Basic functionality

# **** optional exercise! ***** #
# 1) Change the dog breed selectInput() to radio buttons. Since there are a lot of breeds, just make the options Mischling klein, Chihuahua, Labrador Retriever, and Jack Russel Terrier (the four most popular breeds)
# 2) See how the dogs dataframe is filtered the same way in each output object? Turn the filtered dogs dataframe into a reactive object instead, using rdogs <- reactive(). Remember that when you call a reactive object, you have to put open and closed parens after it, e.g., my_object()

library(tidyverse)
library(kableExtra)
library(shiny)
library(here)
library(lubridate)

BabyData<-read_csv(here("Week_10", "Data", "HatchBabyExport.csv")) 
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


# Define UI
ui <- fluidPage(
    
    # Application title
    titlePanel("Twin Baby Weights over Time"),
    
    # Sidebar with a dropdown menu for breed
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId = "Baby Name",
                label = "Baby Name:",
                choices = unique(BabyWeight$`Baby Name`),
                selected = "" # default selection
            ), 
        ),
        
        # Show a plot of the city-wide distribution
        mainPanel(
            column(
                6, # column() modifies the layout (# is the column width)
                h4("Amount eaten (g) over time"),
                plotOutput("distPlot")
            ),
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
            filter(`Baby Name` == input$Baby Name & !is.na(Amount)) %>%
            ggplot(aes(x = factor(Amount))) +
            xlab("District") +
            ylab("Number of dogs") +
            ggtitle(paste("Count of", input$breed, "\n in each district", sep = " ")) +
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
