## Import libraries
library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)

## Read and transform data 
vic_accident <- read_csv("Victoria_Accident_Data_FIT5147S12024PE2v2.csv")  

## Data transformation for vis1
vis1_data <- vic_accident%>%
  group_by(LIGHT_CONDITION_DESC, SPEED_ZONE)%>%
  summarise(COUNT = n(), .groups = 'drop') # override with .groups

## Data transformation for vis 2
vic_accident <- vic_accident%>%
  mutate(HOURS = hour(hms(ACCIDENT_TIME)))%>%
  mutate(HOURS = factor(HOURS, levels = 0:23, ordered = TRUE)) 
# reference: https://stackoverflow.com/questions/40345291/r-extract-hour-from-variable-format-timestamp

vis2_data <- vic_accident%>%
  filter(SPEED_ZONE %in% c("100", "060", "080", "050"))%>% # reference: https://www.geeksforgeeks.org/filter-data-by-multiple-conditions-in-r-using-dplyr/
  group_by(SPEED_ZONE, HOURS)%>%
  summarise(COUNT = n(), .groups = 'drop')

## Data transformation for Map 
map_data <- vic_accident%>%
  mutate(DAYNIGHT = case_when(
    LIGHT_CONDITION_DESC %in% c("Dark No street lights", "Dark Street lights on", "Dark Street lights off", "Dark Street lights unknown", "Unk.") ~ "night",
    LIGHT_CONDITION_DESC %in% c("Day")~"day",
    LIGHT_CONDITION_DESC %in% c("Dusk/Dawn")~"dusk/dawn"))

colors <- c("day" = "green", "dusk/dawn" = "purple", "night" = "blue")
color_palette <- colorFactor(palette = colors, domain = map_data$DAYNIGHT)

# Define UI
ui <- fixedPage(
  titlePanel(
    h1("Victoria Accident Visulisation", align = "center")
  ),
  
  # Vis 1&2
  fixedRow(
    column(7, plotOutput("vis1")),
    column(5, plotOutput("vis2"))
  ),
  
  # Descriptions and interpretations for Vis 1&2
  # reference: https://www.w3schools.com/w3css/w3css_containers.asp
  div(class = "vis-desc",
      fixedRow(
        column(7, 
               h3("Visulisation 1 Description"),
               p(HTML("
                 The stacked bar chart showed the distribution of accidents among different speed zones, and for each speed zone, the proportion of accidents attributed to different light conditions.<br><br>
                 Overall, the top four light conditions for accidents to happen are day, dark no street lights, dusk/dawn, dark street lights on, and the top four speed zones are 100, 60, 80 and 50.
                 "))),
        column(5, 
               h3("Visulisation 2 Description"),
               p(HTML("
               The stacked bar chart illustrates how the number of accidents are distributed across the top four risky speed zones. For each speed zone, there is breakdown of accident counts by the hour.<br><br> 
               From the graph, we can see that most accidents happen during day time, from 8am to 6pm. However, for speed zone 100, there are relatively more accidents before sunrise, and for speed zone 50 and 100, after sunset.<br><br>")))
      )
  ),
  
  # Map
  # reference: https://stackoverflow.com/questions/36469631/how-to-get-leaflet-for-r-use-100-of-shiny-dashboard-height
  # initially i used percentage for width and height and the image wasn't showing
  # so i manually set the pixel
  fixedRow(
    column(12, leafletOutput("map", height = "250px"))
  ),
  
  # Range Slider
  fluidRow(
    column(6, sliderInput("severity_range", "Filter by Severity Rank: ",
                           min = 1, max = 3, value = c(1, 3)))
  ),
  
  
  # Description of the Map
  div(class = "map-desc",
      fixedRow(
        column(12, 
               h3("Map Description"),
               p(HTML("
                According to the map, the majority of the most severe accidents happened during the night. The second level accidents have similar probability to happen during day or night, with day having a greater share, but the difference decreases for third level accidents. <br><br>
                Overall, most accidents happen during day or night, with day having a slight edge, and in all scenarios, the dusk/dawn take the least proportion. 
                  ")))
      )
  ),
  
  # Data Source
  div(class = "data-source",
      h4("Data Source"),
      p(HTML("The dataset used for Programming Exercise: R is based on the Vehicle Road Crash Data 
      released by the Victorian Department of Transport and Planning in January 2024, 
      the original dataset can be found via this link: https://discover.data.vic.gov.au/dataset/victoria-road-crash-data <br><br>
      The adapted dataset has 924 rows and 13 columns, contains numerical, nominal, ordinal and datetime data types."))
  )
)

# Define server logic
server <- function(input, output) {
  # Visulisation 1
  output$vis1 <- renderPlot({
    # Data Visulisation 1
    ggplot(vis1_data, aes(x = SPEED_ZONE, y = COUNT, fill = LIGHT_CONDITION_DESC))+
      geom_bar(stat = "identity", position = "stack")+
      labs(title = "Accidents VS  Speed Zone & Light Condition", x = "Speed Zone", y = "Count of Accidents", fill = "Light Condition")
  })
  
  
  # Visulisation 2
  output$vis2 <- renderPlot({
    # Data Visulisation 2
    ggplot(vis2_data, aes(x = SPEED_ZONE, y = COUNT, fill = HOURS))+
      geom_bar(stat = "identity", position = "stack")+
      labs(title = "Accidents VS  Speed Zone & Hour", x = "Speed Zone", y = "Count of Accidents", fill = "Hour")
  })
  
  # Map
  output$map <- renderLeaflet({
    # Filter data based on input
    filtered_data <- map_data%>%
      filter(SEVERITY_RANK >= input$severity_range[1],
             SEVERITY_RANK <= input$severity_range[2])
    # reference: https://stackoverflow.com/questions/70179179/r-slider-range-input-shiny
      
    # Map visualisation
    leaflet(data = filtered_data)%>% 
      addTiles()%>% 
      setView(lng = 145.465783, lat = -38.482461, zoom = 10)%>%
      addCircleMarkers(
        lng = ~LONGITUDE,
        lat = ~LATITUDE,
        radius = ~7 - SEVERITY_RANK,
        fillColor = ~color_palette(DAYNIGHT),
        color = "lightgray",
        fillOpacity = 1,
        popup = ~paste(
          "Accident Date: ", ACCIDENT_DATE, "<br>",
          "Accident Type: " , ACCIDENT_TYPE_DESC,"<br>", 
          "Light Condition: ",  LIGHT_CONDITION_DESC,"<br>", 
          "Road Geometry: ", ROAD_GEOMETRY_DESC,"<br>", 
          "Speed Zone: ", SPEED_ZONE))%>%
      addLegend(
        position = "bottomright",
        colors = c("green", "purple", "blue"),
        labels = c("day", "dusk/dawn", "night"),
        title = "Day Night Condition")
  })
}

# Run the application
shinyApp(ui = ui, server = server)