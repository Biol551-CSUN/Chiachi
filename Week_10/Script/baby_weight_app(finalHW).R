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

# load libraries
library(tidyverse)
library(kableExtra)
library(shiny)
library(here)
library(lubridate)


BabyData<-read_csv(here("Week_10", "Data", "HatchBabyExport.csv"))  #pull in data I will be using

BabyWeight <- BabyData %>%
    filter(Activity == "Weight")%>% #filter out the weight
    separate(col = `Start Time`, #fix the date to be usable
             into = c("Date", "Time"), #separating the columns into date and time
             sep = " ")%>% #separate by a space (already there)
    separate(col = Date, # make date into three separate columns
             into = c("Day", "Month", "Year"),  # there are the three column names
             sep = "/")%>% #separate by a /
    mutate(BabyWeight, Date = paste(Year, Month, Day, sep = "-"))%>% # mutate the date into a usable date
    select(-Time, -'End Time', -Percentile, -Duration, -Info, -Notes, -X10, -Day, -Month, -Year) # remove columns that we dont need


BabyFeeding <- BabyData %>% #pull the data we need 
    filter(Activity == "Feeding")%>% #filter out feeding, i dont end up using this data anyway
    separate(col = `Start Time`,  #change the column into two
             into = c("Date", "Time"), #rename those two columns
             sep = " ")%>% #separate them by a space
    separate(col = Date, #change the Date column into three separate columns
             into = c("Day", "Month", "Year"), #these are the three column names
             sep = "/")%>% #separate by a / like we did before
    select(-Time, -'End Time', -Percentile, -Duration, -Notes, -X10, -Year) #remove the columns that we dont need

view(BabyWeight)
# Define UI
ui <- fluidPage( #we are first setting up our shiny app 
    
    # Application title
    titlePanel("Twin Baby Stats"), #name what the app will be titled 
    
    # Sidebar with a dropdown menu for Baby names (this time we are using radio buttons)
    sidebarLayout(
        sidebarPanel(
            radioButtons(
                inputId = "Baby Name", #this is what we are using as our radio buttons
                label = "Baby Name:",
                choices = c(
                    "Blakely", #these are our two choices
                    "Micah"
                ),
                selected = "Blakely" #select one of them as the default
            ),
        ),
        
        # Show a plot of the weight over time for each baby 
        mainPanel(
            column(
                6,
                h4("Weight over time for each baby"),
                plotOutput("birthdayPlot") #use the birthday plot? not sure why but it works!
            )
        ) # /mainPanel
    )
) # /fluidPage


# Server logic
server <- function(input, output) { #put in the info for what the shiny app is using to make a plot
    rbabies <- reactive(BabyWeight %>%
                          filter(`Baby Name` == input$`Baby Name`)) #tell the server what to use when filtering out baby names
    
    output$birthdayPlot <- renderPlot({ #render the plot! (make it!)
        rbabies() %>% #the data we will use
            ggplot(aes(  # tell the ggplot what x and y values to use
                x = factor(Date),
                y = Amount
            )) +
            geom_point() + #make it a geompoint (this isnt the prettiest, but it works...)
            xlab("Date") + #label your x values
            ylab("Weight of Baby") + #label your y values
            theme_classic(base_size = 16)+ #classic theme
            theme(axis.title = element_text(size = 13),  # make axis font larger
                    axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1), # change the angle of the x axis text to see it all
                    plot.title = element_text(hjust = 0.5)) # center the plot title 
    })
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
