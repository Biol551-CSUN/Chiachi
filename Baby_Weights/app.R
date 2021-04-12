#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# **** solutions! ***** #
# 1) Change the dog breed selectInput() to radio buttons.
# 2) See how the dogs dataframe is filtered the same way in each output obejct? Turn the filtered dogs dataframe into a reactive object instead, using rdogs <- reactive().

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


BabyFeeding <- BabyData %>%
    filter(Activity == "Feeding")%>%
    separate(col = `Start Time`, 
             into = c("Date", "Time"), 
             sep = " ")%>%
    separate(col = Date, 
             into = c("Day", "Month", "Year"), 
             sep = "/")%>%
    select(-Time, -'End Time', -Percentile, -Duration, -Notes, -X10, -Year)

view(BabyWeight)
# Define UI
ui <- fluidPage(
    
    # Application title
    titlePanel("Twin Baby Stats"),
    
    # Sidebar with a dropdown menu for breed
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                inputId = "Baby Name",
                label = "Baby Name:",
                choices = c(
                    "Blakely",
                    "Micah"
                ),
                selected = "Blakely"
            ),
        ),
        
        # Show a plot of the city-wide distribution
        mainPanel(
            column(
                6,
                h4("Weight over time for each baby"),
                plotOutput("birthdayPlot")
            )
        ) # /mainPanel
    )
) # /fluidPage


# Server logic
server <- function(input, output) {
    rbabies <- reactive(BabyWeight %>%
                          filter(`Baby Name` == input$`Baby Name`))
    
    output$birthdayPlot <- renderPlot({
        rbabies() %>%
            ggplot(aes(
                x = factor(Date),
                y = Amount
            )) +
            geom_point() +
            xlab("Date") +
            ylab("Weight of Baby") +
            theme_classic(base_size = 16)+
            theme(axis.title = element_text(size = 13),  # make axis font larger
                    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1), # change the angle of the x axis text to see it all
                    plot.title = element_text(hjust = 0.5)) # center the plot title 
    })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
