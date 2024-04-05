library(shiny)
library(DT)
library(ggplot2)
library(readr)
library(plotly)
library('this.path')

airline_data <- read_csv(file.path(this.dir(),"Airline_Dataset.csv"))


# Define the UI
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  titlePanel("Airline Customer Data Visualizations", windowTitle = "Airline Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h3("Filter Options", style = "color: #000;"), # Header with white color
      wellPanel(
        h4("Demographics"),
        sliderInput("ageRange", "Age Range:",
                    min = min(airline_data$Age), max = max(airline_data$Age),
                    value = c(min(airline_data$Age), max(airline_data$Age))),
        selectInput("classSelect", "Class:",
                    choices = unique(airline_data$Class), selected = unique(airline_data$Class)[1])
      ),
      wellPanel(
        h4("Travel Details"),
        selectInput("travelTypeSelect", "Type of Travel:",
                    choices = unique(airline_data$`Type of Travel`), selected = unique(airline_data$`Type of Travel`)[1]),
        sliderInput("flightDistanceRange", "Flight Distance Range:",
                    min = min(airline_data$`Flight Distance`), max = max(airline_data$`Flight Distance`),
                    value = c(min(airline_data$`Flight Distance`), max(airline_data$`Flight Distance`)))
      ),
      actionButton("resetButton", "Reset Filters")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Satisfaction",
                 div(class="tab-description", "This plot shows customer satisfaction levels."),
                 plotOutput("satisfactionPlot")
        ),
        tabPanel("Age Distribution",
                 div(class="tab-description", "Age distribution of customers."),
                 plotOutput("ageDistributionPlot")
        ),
        tabPanel("Flight Distance",
                 div(class="tab-description", "Distribution of flight distances."),
                 plotOutput("flightDistancePlot")
        ),
        tabPanel("Departure Delays",
                 div(class="tab-description", "Overview of departure delay times."),
                 plotOutput("departureDelayPlot")
        ),
        tabPanel("Data Table",
                 div(class="tab-description", "Explore the dataset."),
                 DTOutput("table_view"),
                 downloadButton("downloadData", "Download Data", class="download-button")
        ),
        tabPanel("Interactive Plot",
                 div(class="tab-description", "Interactive visualization of flight data."),
                 plotlyOutput("interactivePlot")
        ),
        tabPanel("Flight Distance vs Satisfaction",
                 div(class="tab-description", "Correlation between flight distance and customer satisfaction."),
                 plotOutput("distanceVsSatisfactionPlot")
        ),
        tabPanel("Arrival Delays",
                 div(class="tab-description", "Insights into arrival delays."),
                 plotOutput("arrivalDelaysPlot")
        ),
        tabPanel("Feature Importance",
                 plotOutput("feature_importance_plot")),
        tabPanel("Predict Satisfaction",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput("seat_comfort", "Seat Comfort",
                                 choices = c("Poor" = 1, "Good" = 3, "Excellent" = 5)),
                     selectInput("inflight_entertainment", "Inflight Entertainment",
                                 choices = c("Poor" = 1, "Good" = 3, "Excellent" = 5)),
                     selectInput("online_support", "Online Support",
                                 choices = c("Poor" = 1, "Good" = 3, "Excellent" = 5)),
                     selectInput("customer_type", "Customer Type",
                                 choices = c("Loyal Customer" = 1, "Disloyal Customer" = 0)),
                     selectInput("ease_of_online_booking", "Ease of Online Booking",
                                 choices = c("Poor" = 1, "Good" = 3, "Excellent" = 5)),
                     actionButton("predict", "Predict Satisfaction")
                   ),
                   mainPanel(
                     textOutput("prediction_result")
                   )
                 ))
      )
    )
  )
)
