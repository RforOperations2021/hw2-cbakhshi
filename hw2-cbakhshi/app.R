#importing required libraries---------------------------------------
library(shiny)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(tools)
library(DT)
library(ggplot2)
library(tidyverse)

#importing required data---------------------------------------
airport.data = read.csv("Airport_Monthly_Operational_Report.csv")

#creating a vector with operations columns
operations.vector = c("Month", "Year", "Air.Carrier.Operations",
                      "Commuter.and.Air.Taxi.Operations", 
                      "Military.Operations", "General.Aviation..Total.Operations",
                      "Total.Operations")

#creating a vector with cargo columns
cargo.vector = c("Month", "Year", "Mail.Totals", "Cargo.Totals",
                 "Belly.Freight.Totals", "Cargo.Totals..Cargo...Mail...Belly.Freight.")

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
                                         , tabName = "dashboard"),
                                
                                menuItem(text ="Operations", icon = icon("plane-departure")
                                         , tabName = "operations"),
                                menuItem(text ="Cargo", icon = icon("truck-loading")
                                         , tabName = "cargo")
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
                
        ),
        # Operations page ----------------------------------------------
        tabItem("operations",
                fluidRow(
                    box(plotlyOutput("operations.lp1"), width = 8),
                    box(
                        title= "Choose Inputs",
                        br(),
                        # Select year for plot----------------------------------
                        selectInput(inputId = "operations.year", 
                                    label = "Choose Year:",
                                    choices = unique(airport.data$Year), 
                                    selected = "2019"),
                        
                        # Select y axis for plot----------------------------------
                        selectInput(inputId = "y", 
                                    label = "Choose value for y-axis:",
                                    choices = c("Air Carrier Operations" = "Air.Carrier.Operations",
                                                "Commuter and Air Taxi Operations" = "Commuter.and.Air.Taxi.Operations",
                                                "Military Operations" = "Military.Operations",
                                                "General Aviation: Total Operations" = "General.Aviation..Total.Operations"), 
                                    selected = "Air Carrier Operations"),
                        
                        "Note: The total Operations are shown with a Blue line",
                        
                        br(),
                        
                        # Show data table ---------------------------------------------
                        checkboxInput(inputId = "show_data1",
                                      label = "Show data table",
                                      value = TRUE)
                        , width = 4),
                    
                    # Box with data table 
                    box(DT::dataTableOutput(outputId = "operations.table"), width = 12)
                )
        ),
        
        # Cargo page ----------------------------------------------
        tabItem("cargo",
                fluidRow(
                    box(plotlyOutput("cargo.lp1"), width = 8),
                    box(
                        title= "Choose Inputs",
                        br(),
                        # Select year for plot----------------------------------
                        selectInput(inputId = "cargo.year", 
                                    label = "Choose Year:",
                                    choices = unique(airport.data$Year), 
                                    selected = "2019"),
                        
                        # Select y axis for plot----------------------------------
                        selectInput(inputId = "cargo.y", 
                                    label = "Choose value for y-axis:",
                                    choices = c("Mail Totals" = "Mail.Totals",
                                                "Cargo Totals" = "Cargo.Totals",
                                                "Belly Freight Totals" = "Belly.Freight.Totals"), 
                                    selected = "Mail Totals"),
                        
                        "Note: The total Cargo including Cargo, Mail, and Belly Freights are shown with a Blue line",
                        
                        br(),
                        
                        # Show data table ---------------------------------------------
                        checkboxInput(inputId = "show_data2",
                                      label = "Show data table",
                                      value = TRUE)
                        , width = 4),
                    
                    # Box with data table 
                    box(DT::dataTableOutput(outputId = "cargo.table"), width = 12)
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
    
    # Create a subset of data filtering for selected years for operations tab------
    airport.year <- reactive({
        req(input$operations.year) # ensure availability of value before proceeding
        filter(airport.data, Year %in% input$operations.year)
    })
    
    # Operations trends line plot on the dashboard page
    output$operations.lp1 <- renderPlotly({
        ggplot(airport.year(), aes_string(x = "Month")) +
            geom_line(aes_string(y = "Total.Operations", group= "Year"), color = "blue")  +
            geom_line(aes_string(y = input$y, group = "Year")) + 
            labs(x = "Months", y = toTitleCase(str_replace_all(input$y, ".", " ")), 
                 title ="Total Operations Vs Air Carrier, Military, Taxi, and General Aviation") +
            theme_bw() + scale_x_discrete(limits = month.name)
    })
    # Print data table if checked -------------------------------------
    output$operations.table <- DT::renderDataTable(
        if(input$show_data1){
            DT::datatable(data = airport.year()[, operations.vector], 
                          options = list(pageLength = 5), 
                          rownames = FALSE)
        }
    )
    
    # Create a subset of data filtering for selected years for cargo tab------
    airport.year <- reactive({
        req(input$cargo.year) # ensure availability of value before proceeding
        filter(airport.data, Year %in% input$cargo.year)
    })
    
    # cargo trends line plot on the dashboard page
    output$cargo.lp1 <- renderPlotly({
        ggplot(airport.year(), aes_string(x = "Month")) +
            geom_line(aes_string(y = "Cargo.Totals..Cargo...Mail...Belly.Freight.",
                                 group= "Year"), color = "blue")  +
            geom_line(aes_string(y = input$cargo.y, group = "Year")) + 
            labs(x = "Months", y = toTitleCase(str_replace_all(input$y, ".", " ")), 
                 title ="Total Cargo (Mail + Cargo + Belly Freights) Vs Mail, just Cargo, and Belly Freights") +
            theme_bw() + scale_x_discrete(limits = month.name)
    })
    # Print data table if checked -------------------------------------
    output$cargo.table <- DT::renderDataTable(
        if(input$show_data2){
            DT::datatable(data = airport.year()[, cargo.vector], 
                          options = list(pageLength = 5), 
                          rownames = FALSE)
        }
    )
}  
# Run the application 
shinyApp(ui = ui, server = server)
