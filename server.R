library(shiny)
library(DT)
library(ggplot2)
library(readr)
library(plotly)
library(bslib)
library(xgboost)
library('this.path')

airline_data <- read_csv(file.path(this.dir(),"Airline_Dataset.csv"))
xgb_model <- readRDS("xgb_model.rds")
importance_matrix <- readRDS("importance_matrix.rds")
importance_matrix_df <- as.data.frame(importance_matrix)
confusion_matrix <- readRDS("confusion_matrix.rds")
confusion_matrix_df <- as.data.frame(as.table(confusion_matrix))
names(confusion_matrix_df) <- c("Actual", "Predicted", "Frequency")
print(confusion_matrix_df)
# Define server logic
server <- function(input, output, session) {
  
  observe(session$setCurrentTheme(
    if(isTRUE(input$dark_mode)){
      bs_theme(bootswatch = "cyborg")
    } else {
      bs_theme(bootswatch = "cosmo")
    }
  ))
  
  
  output$feature_importance_plot <- renderPlot({
    # Assuming importance_matrix is already a dataframe with columns Feature and Importance
    ggplot(importance_matrix, aes(x = reorder(Feature, Importance), y = Importance)) +
      geom_col(fill = "steelblue") +
      coord_flip() +  # Flip coordinates to make the plot horizontal
      theme_minimal() +
      labs(title = "Feature Importance",
           y = "Importance Score", x = "Features") +
      theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            axis.title.x = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12))
  })
  
  plot_confusion_matrix <- function(df) {
    ggplot(data = df, aes(x = Actual, y = Predicted, fill = Frequency)) +
      geom_tile() +
      geom_text(aes(label = Frequency), vjust = 1.5, color = "white") +
      scale_fill_gradient(low = "blue", high = "red") +
      labs(x = "Actual", y = "Predicted", fill = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
  
  output$confMatrixPlot <- renderPlot({
    plot_confusion_matrix(confusion_matrix_df)
  })
  
  prediction <- eventReactive(input$predict, {
    
    
    model_input <- rep(-99, 22)
    
    # Step 3: Assign the rounded averages to their specific positions in the model input
    # The positions are adjusted here based on your indication; for example, "Customer Type" is feature 1
    model_input[1] <- as.factor(input$customer_type)  # Assuming "Customer Type" is at the 1st position in X_test
    model_input[7] <- as.numeric(input$seat_comfort)  # Assuming "Seat comfort" is now the 2nd feature in importance but 7th in the model
    model_input[12] <- as.numeric(input$inflight_entertainment)  # "Inflight entertainment"
    model_input[13] <- as.numeric(input$online_support)  # "Online support"
    model_input[14] <- as.numeric(input$ease_of_online_booking)  # "Ease of Online booking"
    
    # Assuming the rest of the features can be filled with their averages or a default/average value
    # If specific indices are known for other features, they should be assigned similarly
    
    # Convert the model input vector to a matrix format expected by xgboost
    model_input_matrix <- matrix(model_input, nrow = 1)
    
    # Predict using the xgb_model
    prediction <- predict(xgb_model, newdata = model_input_matrix, type = "prob")
    print(model_input_matrix)
    print(prediction)
    
  })
  
  output$prediction_result <- renderText({
    req(prediction())  # Ensure prediction is not NULL
    prob <- prediction()
    result <- ifelse(prob > 0.5, "Satisfied", "Dissatisfied")
    paste("Predicted Passenger Satisfaction: ", result, "\nProbability: ", round(prob, 5))
  })
  
  # Reactive expression for filtered data
  filtered_data <- reactive({
    airline_data %>%
      filter(Age >= input$ageRange[1], Age <= input$ageRange[2],
             `Type of Travel` == input$travelTypeSelect, # Correctly using backticks
             Class == input$classSelect,
             `Flight Distance` >= input$flightDistanceRange[1], `Flight Distance` <= input$flightDistanceRange[2])
  })
  
  # Satisfaction Distribution Plot
  output$satisfactionPlot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = satisfaction)) +
      geom_bar(aes(fill = satisfaction), show.legend = FALSE) +
      theme_minimal() +
      labs(title = "Satisfaction Distribution", x = "Satisfaction", y = "Count") +
      scale_fill_brewer(palette = "Set1")
  })
  
  # Age Distribution Plot
  output$ageDistributionPlot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = Age)) +
      geom_histogram(binwidth = 5, fill = "skyblue", color = "white") +
      theme_minimal() +
      labs(title = "Age Distribution", x = "Age", y = "Count")
  })
  
  # Flight Distance Distribution Plot
  output$flightDistancePlot <- renderPlot({
    ggplot(data = filtered_data(), aes(x = `Flight Distance`)) +
      geom_histogram(binwidth = 500, fill = "cornflowerblue", color = "white") +
      theme_minimal() +
      labs(title = "Flight Distance Distribution", x = "Flight Distance (miles)", y = "Count")
  })
  
  # Departure Delay Distribution Plot
  output$departureDelayPlot <- renderPlot({
    data <- filtered_data()
    mean_delay <- mean(data$`Departure Delay in Minutes`, na.rm = TRUE)  # Calculate mean delay
    
    ggplot(data, aes(x = `Departure Delay in Minutes`)) +
      geom_histogram(binwidth = 30, fill = "tomato", color = "white") +
      geom_vline(xintercept = mean_delay, color = "black", linetype = "dashed") +  # Add mean line
      theme_minimal() +
      labs(title = "Departure Delay Distribution", x = "Departure Delay (minutes)", y = "Count")
  })
  
  # Data Table view
  output$table_view <- renderDT({
    datatable(filtered_data(), options = list(pageLength = 5))
  })
  
  # Download handler for exporting the dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("airline-data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  # Interactive Plot
  output$interactivePlot <- renderPlotly({
    # Use aes_string() to specify variables by name
    plot_ly(data = filtered_data(), x = as.formula(paste0("~`", input$xVariable, "`")), 
            y = as.formula(paste0("~`", input$yVariable, "`")), 
            color = ~satisfaction,
            type = "scatter", mode = "markers", 
            marker = list(size = 10, opacity = 0.8)) %>%
      layout(title = paste(input$xVariable, "vs", input$yVariable),
             xaxis = list(title = input$xVariable),
             yaxis = list(title = input$yVariable),
             legend = list(title = "Satisfaction Level"))
  })
  
  # Flight Distance vs. Satisfaction Scatter Plot
  output$distanceVsSatisfactionPlot <- renderPlot({
    gg <- ggplot(data = filtered_data(), aes(x = `Flight Distance`, y = satisfaction)) +
      geom_point(aes(color = satisfaction)) +  # Color points by satisfaction
      theme_minimal() +
      labs(title = "Flight Distance vs Satisfaction", x = "Flight Distance", y = "Satisfaction")
    print(gg)
  })
  
  # Arrival Delays Distribution Plot
  output$arrivalDelaysPlot <- renderPlot({
    gg <- ggplot(data = filtered_data(), aes(x = `Arrival Delay in Minutes`)) +
      geom_histogram(binwidth = 30, fill = "tomato", color = "white") +  # Adjust binwidth as needed
      theme_minimal() +
      labs(title = "Arrival Delays Distribution", x = "Arrival Delay (minutes)", y = "Count")
    print(gg)
  })
  
  
  # ObserveEvent for reset button logic
  observeEvent(input$resetButton, {
    # Reset filters to their initial values
    updateSliderInput(session, "ageRange", value = c(min(airline_data$Age), max(airline_data$Age)))
    updateSelectInput(session, "travelTypeSelect", selected = unique(airline_data$`Type of Travel`)[1])
    updateSelectInput(session, "classSelect", selected = unique(airline_data$Class)[1])
    updateSliderInput(session, "flightDistanceRange", value = c(min(airline_data$`Flight Distance`), max(airline_data$`Flight Distance`)))
  })
  
}
