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

#reordering factor levels in the Year Column ------------------------------
airport.data$Year = factor(airport.data$Year, levels=c( '2010', '2011', '2012',
                                                        '2013', '2014', '2015',
                                                        '2016', '2017', '2018',
                                                        '2019', '2020'))


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
                         valueBoxOutput("cargo")),
                # Plot ----------------------------------------------
                fluidRow(
                    tabBox(title = "Plots",
                           width = 12,
                           tabPanel(strong("Total Passenger Trends"), plotlyOutput("dashboard.lp1")),
                           tabPanel(strong("Total Operation Trends"), plotlyOutput("dashboard.lp2")),
                           tabPanel(strong("Total Cargo Trends"), plotlyOutput("dashboard.lp3"))
                    )
                )
                
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
    
    
    # passenger trends line plot on the dashboard page
    output$dashboard.lp1 <- renderPlotly({
        ggplot(airport.data, aes_string(x = "Month", y = "Total.Passengers", color = "Year")) +
            geom_line(aes(group=Year)) +
            labs(x = "Months", y ="Total Passengers", 
                 title ="This graph shows the trend in total passengers arrival + departure at the airport") +
            theme_bw() + scale_x_discrete(limits = month.name)
    })
    
    # Operations trends line plot on the dashboard page
    output$dashboard.lp2 <- renderPlotly({
        ggplot(airport.data, aes_string(x = "Month", y = "Total.Operations", color = "Year")) +
            geom_line(aes(group=Year)) +
            labs(x = "Months", y ="Total Operations", 
                 title ="This graph shows the trend in total Operations inlcuding Air Carrier, Military, Taxi, and General Aviation)") +
            theme_bw() + scale_x_discrete(limits = month.name)
    })
    
    # Cargo trends line plot on the dashboard page
    output$dashboard.lp3 <- renderPlotly({
        ggplot(airport.data, aes_string(x = "Month", y = "Cargo.Totals..Cargo...Mail...Belly.Freight.", color = "Year")) +
            geom_line(aes(group=Year)) +
            labs(x = "Months", y ="Total Cargo", 
                 title ="This graph shows the trend in cargo shipments inlcuding Cargo, Mail, and Belly Freight))") +
            theme_bw() + scale_x_discrete(limits = month.name)
    })
}  
# Run the application 
shinyApp(ui = ui, server = server)