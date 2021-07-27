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
  
    print(cities_max_bike)
  
  print(factor(cities_max_bike$BIKE_PREDICTION_LEVEL))
  myFirstFun<-function(cities_max_bike)
  {
    
    if(cities_max_bike$BIKE_PREDICTION_LEVEL=='small')
    {
      
      mapcol="green"
      mapradius=6
    }
    
    else if(cities_max_bike$BIKE_PREDICTION_LEVEL=='medium')
    {
      mapcol="yellow"
      mapradius=10
    }
    
    else
    {
      mapcol="red"
      mapradius=12
    }
    return(mapcol)
  }
  
  
  myFirstFun1<-function(cities_max_bike)
  {
    
    if(cities_max_bike$BIKE_PREDICTION_LEVEL=='small')
    {
      
      
      mapradius=6
    }
    
    else if(cities_max_bike$BIKE_PREDICTION_LEVEL=='medium')
    {
      
      mapradius=10
    }
    
    else 
    {
      
      mapradius=12
    }
    return(mapradius)
  }
  
  # Then render output plots with an id defined in ui.R
  output$city_bike_map <- renderLeaflet({
    
    # Complete this function to render a leaflet map
    color_levels1 <- colorFactor(palette=c("green", "yellow", "red"),domain=cities_max_bike$BIKE_PREDICTION_LEVEL) 
    
    print(~color_levels(cities_max_bike$BIKE_PREDICTION_LEVEL))
    map <- leaflet(cities_max_bike) %>% addTiles()  %>% 
      addCircleMarkers(lng = cities_max_bike$LNG, lat = cities_max_bike$LAT,color=myFirstFun(cities_max_bike)
                       ,radius=myFirstFun1(cities_max_bike),popup=cities_max_bike$LABEL)
    
  })
  
  
  # If All was selected from dropdown, then render a leaflet map with circle markers
  # and popup weather LABEL for all five cities
  observeEvent(input$city_dropdown, {
  if(input$city_dropdown != 'All') {
      
    leafletProxy("city_bike_map") %>% clearShapes()
    index = which(cities_max_bike$CITY_ASCII == input$city_dropdown)
    leafletProxy("city_bike_map")%>% addCircles(lng = cities_max_bike$LNG[index], 
                                                lat = cities_max_bike$LAT[index],
                                                popup = cities_max_bike$DETAILED_LABEL[index])
    
     output$temp_line <- renderPlot({
      ggplot(city_weather_bike_df, aes(x = city_weather_bike_df$    , y = city_weather_bike_df$TEMPERATURE) +
        geom_line() +
        geom_point() +
        geom_text()
    })
    
  }
  else {
    
    leafletProxy("city_bike_map")
    
  } 
 })
})
