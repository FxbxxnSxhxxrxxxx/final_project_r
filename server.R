# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
require(plotly)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")


test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){

  city_weather_bike_df <- test_weather_data_generation()
  
  # Create the cities_max_bike data frame with all cities
  cities_max_bike <- city_weather_bike_df %>%
    select(BIKE_PREDICTION, BIKE_PREDICTION_LEVEL, CITY_ASCII, LNG, LAT, LABEL, DETAILED_LABEL) %>%
    rename(bike_prediction = BIKE_PREDICTION, bike_prediction_level = BIKE_PREDICTION_LEVEL, city_name = CITY_ASCII)
  # Add new columns for color and size
  cities_max_bike <- cities_max_bike %>%
    mutate(colors = case_when(bike_prediction_level == "small" ~ "green",
                              bike_prediction_level == "medium" ~ "yellow",
                              bike_prediction_level == "large" ~ "red"),
           sizes = case_when(bike_prediction_level == "small" ~ 6,
                             bike_prediction_level == "medium" ~ 10,
                             bike_prediction_level == "large" ~ 12))
  # Filter the row(s) with the maximum bike_prediction
  cities_max_bike <- cities_max_bike %>% 
    group_by(city_name) %>%
    filter(bike_prediction == max(bike_prediction))
  
  # Observe drop-down event
  observeEvent(input$city_dropdown, {
    if(input$city_dropdown == 'All') {
      # If All was selected from dropdown, then render a leaflet map with circle markers
      # and popup weather LABEL for all five cities
      # Then render output plots with an id defined in ui.R
      output$city_bike_map <- renderLeaflet({
        
        # Create the leaflet map
        leaflet(data = cities_max_bike) %>%
          addTiles() %>%
          addCircleMarkers(
            lng = ~LNG,
            lat = ~LAT,
            color = ~colors,
            fillColor = ~colors,
            radius = ~sizes,
            popup = ~LABEL
          )
        
      })
    }
    else {
      # If just one specific city was selected, then render a leaflet map with one marker
      # on the map and a popup with DETAILED_LABEL displayed
      # Then render output plots with an id defined in ui.R
      output$city_bike_map <- renderLeaflet({
        
        city_df <- filter(cities_max_bike, city_name == input$city_dropdown)
        
        # Create the leaflet map
        leaflet(data = city_df) %>%
          addTiles() %>%
          addCircleMarkers(
            lng = ~LNG,
            lat = ~LAT,
            color = ~colors,
            fillColor = ~colors,
            radius = ~sizes,
            popup = ~DETAILED_LABEL
          )
      })
      
      # Render bike-sharing demand prediction trend plot
      output$temp_line <- renderPlot({
        
        city_weather_df <- filter(city_weather_bike_df, CITY_ASCII == input$city_dropdown)
        # Convert timestamp to POSIXct format
        city_weather_df$FORECASTDATETIME <- as.POSIXct(city_weather_df$FORECASTDATETIME, format = "%Y-%m-%d %H:%M:%S")
        # Get the current time
        now <- Sys.time()
        # Calculate the timestamp of three hours from now
        three_hours <- now + 3*60*60
        # Subset the data frame to include rows with timestamp within the next three hours
        df_next_three_hours <- subset(city_weather_df, FORECASTDATETIME >= now & FORECASTDATETIME <= three_hours)
        
        ggplot(data = city_weather_df, aes(x = FORECASTDATETIME, y = TEMPERATURE )) +
          geom_line(color = "yellow") +
          geom_point(color = "orange") +
          geom_text(data = city_weather_df[seq(1, nrow(city_weather_df), 4), ], 
                    aes(label = TEMPERATURE, nudge_x = 1.5))
      })
      
      # Render bike-sharing demand prediction trend plot
      output$bike_line <- renderPlot({
        
        city_weather_df <- filter(city_weather_bike_df, CITY_ASCII == input$city_dropdown)
        # Convert timestamp to POSIXct format
        city_weather_df$FORECASTDATETIME <- as.POSIXct(city_weather_df$FORECASTDATETIME, format = "%Y-%m-%d %H:%M:%S")
        # Get the current time
        now <- Sys.time()
        # Calculate the timestamp of three hours from now
        three_hours <- now + 3*60*60
        # Subset the data frame to include rows with timestamp within the next three hours
        df_next_three_hours <- subset(city_weather_df, FORECASTDATETIME >= now & FORECASTDATETIME <= three_hours)
        
        ggplot(data = city_weather_df, aes(x = FORECASTDATETIME, y = BIKE_PREDICTION )) +
          geom_line(color = "yellow") +
          geom_point(color = "orange") +
          geom_text(data = city_weather_df[seq(1, nrow(city_weather_df), 4), ], 
                    aes(label = BIKE_PREDICTION, nudge_x = 1.5))
      })
      
      # Function to create plot
      create_plot <- function(data) {
        ggplot(data, aes(x = HUMIDITY, y = BIKE_PREDICTION)) +
          geom_point() +
          geom_smooth(method = "lm", formula = y ~ poly(x, 4))
      }
      
      # Render plot
      output$humidity_pred_chart <- renderPlot({
        
        city_weather_df <- filter(city_weather_bike_df, CITY_ASCII == input$city_dropdown)
        
        create_plot(city_weather_df)
      })
      
    }
    
  })
  
})
