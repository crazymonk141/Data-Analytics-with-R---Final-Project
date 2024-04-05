### Shiny

suppressPackageStartupMessages({
  library(shiny)
  library(shinyWidgets)
  library(leaflet)
  library(plotly)
  library(DT)
  library(shinythemes)
  library(tidyverse)
})

load('E:/RUBEN/Dropbox/ESADE/MIBA/DAR/DATA/shiny_adria/DATA/data_final.RData')

#User Interface
ui <- fluidPage(
  # Custom CSS to position the logo (picture has to be stored at www folder inside parent directory)
  tags$head(
    tags$style(HTML(
      ".logo-container {
        position: absolute;
        top: -10px;
        right: 10px;
      }"
    ))
  ),
  
  # Logo container with custom class
  div(class = "logo-container",
      # logo image
      img(src = "Esade.png", width = 100, height = 100)
  ),
  titlePanel("MiBA 2024: Students background in-depth"),
  # shinythemes::themeSelector(), # If we want to have a dropdown of styles to check.
  theme = shinytheme("united"), # Here, we select one outright.
  sidebarLayout(
    sidebarPanel(
      width = 2,
      sliderInput(inputId = "age", label = "Select the range of ages", min = min(data_final$age), max = max(data_final$age), value = c(min(data_final$age), max(data_final$age)), step = 1),
      checkboxGroupInput(inputId = "section", label = "Select Section", choices = sort(unique(data_final$section)), selected = unique(data_final$section)),
      # checkboxGroupInput("nationality", "Select Nationalities", unique(data_final$nationality), selected = unique(data_final$nationality))
      # selectInput(inputId = "nationality", label = "Select Nationalities", choices = unique(data_final$nationality), selected = NA, multiple = TRUE)
      pickerInput(inputId = "nationality", label = "Select Nationalities", choices = sort(unique(data_final$nationality)), selected = NA, multiple = TRUE, options = list(
        `actions-box` = TRUE))
    ),
    mainPanel(
      width = 10,
      leafletOutput(outputId = "map"),
      tabsetPanel(
        tabPanel(title = "Field of Studies by Pre-program",
                 plotlyOutput(outputId = "plot_facet")),
        tabPanel(title = "Age Distribution by Field of Study",
                 plotOutput(outputId = "plot_ages")),
        tabPanel(title = "Data",
                 DTOutput(outputId = "data"))
      )
    )
  )
)


server <- function(input, output, session) {
  #We set a reactive element that will help us in reusing a computation element
  #across multiple outputs (in this case, the dataframe based on the inputs).
  raw_data <- reactive({
    data_final %>% 
      filter(age >= input$age[1] & age <= input$age[2]) %>%
      filter(nationality %in% input$nationality) %>%
      filter(section %in% input$section)
  })
  
  # Frequency plot (plotly) ----
  # (see how we use our reactive element piped, we will keep doing so)
  output$plot_facet <- renderPlotly({
    raw_data() %>%
      ggplot(mapping = aes(x = field_of_studies, fill = pre_program)) +
      geom_bar() +
      theme_minimal() + 
      labs(
        title = "Barplot: Pre-Program by Field of Studies",
        x = "Field of Studies", 
        y= "Pre-Program Totals")
  })
  
  # Age plot (ggplot2) ----
  output$plot_ages <- renderPlot({
    raw_data() %>%
      ggplot(mapping = aes(x = age, color = pre_program, fill = pre_program)) +
      geom_density(alpha = 0.5) +
      labs(fill = "Field of Studies", color = "Field of Studies") +
      theme_minimal()
  })
  
  # Data table (DT) ----
  output$data <- renderDT({
    raw_data() %>%
      select(-longitude, -latitude) %>% 
      arrange(last_name) %>% 
      datatable(colnames = c('First name', 'Second name', 'Section', 'Gender', 'Age', 'Nationality', 'Field of studies', 'Pre-program'),
                # filter = 'top',
                options = list(pageLength = raw_data() %>% pull(nationality) %>% length(), 
                               autoWidth = TRUE)) %>% 
      formatStyle(columns = colnames(.),
                  `font-size` = "4px")
  })
  
  # Interactive map (leaflet) ----
  output$map <- renderLeaflet({
    
    ESADE_lng <- 2.0915
    ESADE_lat <- 41.4670
    
    nations_map <- raw_data() %>% select(nationality) %>% distinct() %>% pull()
    
    data_leaflet <- data_final %>%
      filter(nationality %in% nations_map) %>%
      group_by(nationality) %>%
      summarise(
        n_students = n(),
        # names_concatenated = names_concatenated,
        nationality = unique(nationality),
        latitude = mean(latitude),
        longitude = mean(longitude)
      )
    
    data_leaflet %>%
      leaflet() %>%
      addTiles() %>%
      setView(lng =  ESADE_lng, lat = ESADE_lat, zoom = 3) %>%
      addCircleMarkers(
        popup = ~ raw_data() %>% 
          unite(col = 'first_last_name', sep = " ", c(first_name, last_name)) %>% 
          pull(first_last_name) %>% 
          paste(collapse = ', '),
        radius = ~ n_students,  # Adjusting the scaling factor with the students size
        fillColor = "red",
        color = "red",
        weight = 2/3
      )
  })
}

shinyApp(ui=ui, server=server)
