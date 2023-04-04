# Load required libraries
require(leaflet)


# Create a RShiny UI
shinyUI(
  fluidPage(padding=5,
  titlePanel("Bike-sharing demand prediction app"), 
  # Create a side-bar layout
  sidebarLayout(
    # Create a main panel to show cities on a leaflet map
    mainPanel(
      # leaflet output with id = 'city_bike_map', height = 1000
      leafletOutput(outputId = "city_bike_map", height = 400, width = 700)
    ),
    # Create a side bar to show detailed plots for a city
    sidebarPanel(
      # select drop down list to select city
      selectInput(
        inputId="city_dropdown", 
        choices = c("All", "Seoul", "Suzhou", "London", "New York", "Paris"),
        label = "City"),
      
      # Plot output
      plotOutput("temp_line", height = 200, width = 350),
      
      # Add plot output with click event
      plotOutput("bike_line", click = "plotly_click", height = 200, width = 350),
      
      # Plot output 
      plotOutput("humidity_pred_chart", height = 200, width = 350)
      
    )
  )
))