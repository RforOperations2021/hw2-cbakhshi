#importing required libraries---------------------------------------
library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(tools)

#importing required data---------------------------------------
airport.data = read.csv("Airport_Monthly_Operational_Report.csv")

# To avoid plotly issues ----------------------------------------------
pdf(NULL)

# Application title ----------------------------------------------
header <- dashboardHeader(title = "Austin Airport Operations", 
                          titleWidth = 300)

# Dashboard sidebar
sidebar <- dashboardSidebar(width = 300,
                            sidebarMenu(
                                id = "tabs",
                                
                                # Menu Items -----------------------------------
                                menuItem(text ="Dashboard", icon = icon("bar-chart")
                                         , tabName = "dashboard")
                            )
)

# Dashboard body ----------------------------------------------
body <- dashboardBody(
    tabItems(
        # Dashboard page ----------------------------------------------
        tabItem("dashboard",
                #Value Boxes ------------------------------------------
                fluidRow(valueBoxOutput("passengers"),
                         valueBoxOutput("operations"),
                         valueBoxOutput("cargo"))
                
        )
    )
)


# Define UI for application 
ui <- dashboardPage(header, sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Making valuebox for Average Passengers Monthly ---------------------------
    output$passengers = renderValueBox({
        avg = round(mean(airport.data$Total.Passengers))
        
        valueBox(subtitle = "Average Monthly Passengers (Arrival + Departure)", 
                 value = avg, icon = icon("users"), color = "blue")
    })
    
    # Making valuebox for Average Total Operations Monthly -------------------
    output$operations <- renderValueBox({
        avg = round(mean(airport.data$Total.Operations))
        
        valueBox(subtitle = "Average Monthly Operations (Air Carrier + Military + Taxi + General Aviation) ", 
                 value = avg, icon = icon("plane-departure"), color = "blue")
    })
    
    # Making valuebox for Average Cargo Monthly ------------------------------
    output$cargo <- renderValueBox({
        avg = round(mean(airport.data$Cargo.Totals..Cargo...Mail...Belly.Freight.))
        
        valueBox(subtitle = "Average Monthly Cargo (Cargo + Mail + Belly Freight)", 
                 value = avg, icon = icon("truck-loading"), color = "blue")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)