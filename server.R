# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")


test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df <- generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){
  # Define a city list
  
  # Define color factor
  color_levels <- colorFactor(c("green", "yellow", "red"), 
                              levels = c("small", "medium", "large"))
  city_weather_bike_df <- test_weather_data_generation()
  
  
  # Create another data frame called `cities_max_bike` with each row contains city location info and max bike
  # prediction for the city
  cities_max_bike <- city_weather_bike_df %>%
    group_by(CITY_ASCII,LAT,LNG,BIKE_PREDICTION,BIKE_PREDICTION_LEVEL,LABEL,
             DETAILED_LABEL,FORECASTDATETIME,TEMPERATURE ) %>%
    summarize(count = n(), 
              max = max(BIKE_PREDICTION, na.rm = TRUE))
  # Observe drop-down event
  
  # Then render output plots with an id defined in ui.R
  output$city_bike_map <- renderLeaflet({
    leaflet(cities_max_bike) %>%
      addTiles() %>%
      addCircleMarkers(lat = cities_max_bike$LAT, 
                       lng = cities_max_bike$LNG, 
                       popup = cities_max_bike$LABEL,
                       color = color_levels,
                       radius = cities_max_bike$BIKE_PREDICTION_LEVEL)
    
  })
  
  
  # If All was selected from dropdown, then render a leaflet map with circle markers
  # and popup weather LABEL for all five cities
  observeEvent(input$city_dropdown, {
    if(input$city_dropdown != 'All') {
      #Render the city overview map
      
    }else {
      #Render the specific city map
    } 
  })
  
  # If just one specific city was selected, then render a leaflet map with one marker
  # on the map and a popup with DETAILED_LABEL displayed
  
})
