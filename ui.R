library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(readr)
library(plotly)
library(bslib)
library('this.path')

airline_data <- read_csv(file.path(this.dir(),"Airline_Dataset.csv"))
columnChoices <- names(airline_data)

# Define the UI
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "cosmo"),
  navbarPage("Airline Dashboard",
             theme = bs_theme(version = 5, bootswatch = "cosmo"),
             header = div(checkboxInput("dark_mode", "Dark mode")),
             tabPanel("Home", includeMarkdown("home.md")),
             tabPanel("Plots",
                          sidebarLayout(
                            sidebarPanel(
                              h3("Filter Options"),
                              wellPanel(
                                h4("X vs Y"),
                                selectInput("xVariable", "X-Axis Variable", choices = columnChoices),
                                selectInput("yVariable", "Y-Axis Variable", choices = columnChoices)
                              ),
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
                                tabPanel("Satisfaction", plotOutput("satisfactionPlot")),
                                tabPanel("Age Distribution", plotOutput("ageDistributionPlot")),
                                tabPanel("Flight Distance", plotOutput("flightDistancePlot")),
                                tabPanel("Departure Delays", plotOutput("departureDelayPlot")),
                                tabPanel("Arrival Delays", plotOutput("arrivalDelaysPlot")),
                                tabPanel("Interactive Plot",div(class="tab-description", "Interactive visualization of flight data."),plotlyOutput("interactivePlot")),
                                tabPanel("Flight Distance vs Satisfaction", plotOutput("distanceVsSatisfactionPlot"))
                              )
                            )
                          )),
                 tabPanel("ML Models",
                          sidebarLayout(
                            sidebarPanel( # Any inputs or filters specific to ML Models
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Feature Importance", plotOutput("feature_importance_plot")),
                                tabPanel("Confusion Matrix", plotOutput("confMatrixPlot")),
                                tabPanel("Data Table", DTOutput("table_view"), downloadButton("downloadData", "Download Data"))
                              )
                            )
                          )),
                 tabPanel("Predictions",
                          sidebarLayout(
                            sidebarPanel(
                              # Inputs for predictions
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
                          )),
             tabPanel("About", includeMarkdown("about.md")),
             
  ))
