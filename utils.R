install.packages("shiny")
install.packages("shinydashboard")
install.packages("leaflet")
install.packages("htmltools")
install.packages("ggplot2")
install.packages("forecast")

# app.R
# Tax Forecasting Website

library(shiny)
library(shinydashboard)
library(leaflet)
library(htmltools)
library(ggplot2)
library(forecast)

# Load tax data
tax_data <- read.csv("/Users/fayolazulkarnaen/Documents/UGM Year 1 Semester 2/R Programming Project Kelompok 8/Laporan Pajak Semester 2 2023(Laporan Pajak 10 tahun).csv", stringsAsFactors = FALSE)

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Tax Visionary"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tax Data", tabName = "tax_data", icon = icon("chart-line")),
      menuItem("Forecast", tabName = "forecast", icon = icon("chart-area"))
    ),
    conditionalPanel(
      condition = "input.tabSelected == 'forecast'",
      selectInput("tax_type",
                  label = "Select Tax Type:",
                  choices = unique(tax_data$TaxType),
                  selected = "Penerimaancean Perpajakan"
      ),
      selectInput("method",
                  label = "Select Forecasting Method:",
                  choices = c("ARIMA", "HoltWinters"),
                  selected = "ARIMA"
      ),
      sliderInput("horizon",
                  label = "Forecast Horizon (Years):",
                  min = 1,
                  max = 5,
                  value = 3,
                  step = 1
      ),
      actionButton("generate_forecast", "Generate Forecast")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "tax_data",
              fluidRow(
                box(
                  title = "Tax Data Visualization",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("tax_plot")
                )
              )
      ),
      tabItem(tabName = "forecast",
              fluidRow(
                box(
                  title = "Forecast Visualization",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("forecast_plot")
                ),
                box(
                  title = "Forecast Results",
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  verbatimTextOutput("forecast_results")
                )
              )
      )
    )
  )
)

TaxType <- c("2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022", "2023")

output$forecast_plot <- renderPlot({
  # Loop through each year in TaxType
  lapply(TaxType, function(year) {
    # Filter your data for the current year
    data_year <- forecast_results[forecast_results$Year == year, ]
    
    # Create a plot for the current year
    ggplot(data_year, aes(x = Year, y = TaxRevenue)) +
      geom_line() +
      labs(title = paste("Tax Revenue for", year)) +
      theme_bw() 
  }) 
})

# Define server logic
server <- function(input, output) {
  
  # Filter data based on selected tax type
  filtered_data <- reactive({
    subset(tax_data, TaxType == input$tax_type)
  })
  
  # Create tax data visualization
  output$tax_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Year, y = Value)) +
      geom_line() +
      labs(title = paste("Tax Data for", input$tax_type),
           x = "Year",
           y = "Tax Revenue (Rp Trillions)") +
      theme_bw()
  })
  
  # Create forecast based on selected method
  forecast_results <- eventReactive(input$generate_forecast, {
    ts_data <- ts(filtered_data()$Value, start = 2014, frequency = 1)
    
    if (input$method == "ARIMA") {
      model <- auto.arima(ts_data)
      forecast(model, h = input$horizon)
    } else if (input$method == "HoltWinters") {
      model <- HoltWinters(ts_data)
      forecast(model, h = input$horizon)
    }
  })
  
  # Show forecast results in text output
  output$forecast_results <- renderPrint({
    summary(forecast_results())
  })
  
  # Create forecast visualization
  output$forecast_plot <- renderPlot({
    autoplot(forecast_results()) +
      labs(title = paste("Forecast for", input$tax_type, "using", input$method),
           x = "Year",
           y = "Tax Revenue (Rp Trillions)") +
      theme_bw()
  })
}

# Run the application
shinyApp(ui = ui, server = server)