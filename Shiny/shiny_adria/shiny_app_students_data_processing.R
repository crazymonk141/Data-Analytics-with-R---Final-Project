library(readxl)
library(tidyverse)
library(data.table)
library(janitor) #Library we use for snake naming of variables

#Data Importing
data <- read_excel("./DATA/shiny_adria/DATA/MIBA Students List_ShinyApp.xlsx", sheet = 1) %>% clean_names()
countries <- read_csv("./DATA/shiny_adria/DATA/country_capital_lat_lon.csv")  %>% clean_names()

map_dbl(.x = data, .f = function (x) {sum(is.na(x))}) # Checking Data missing values

#Data Cleaning

data$nationality[!data$nationality %in% countries$country] #Which namings appearing in data are not in countries? 

data_final <- data %>%
  mutate(nationality= case_when(
    nationality == "UK" ~ "United Kingdom (UK)",
    nationality == "USA" ~ "United States of America",
    .default = nationality
  )) %>%
  left_join(y = select(countries, country, latitude, longitude), by = c("nationality" = "country")) %>%
  select(-degree)

save(data_final, file = './DATA/shiny_adria/DATA/data_final.RData')

