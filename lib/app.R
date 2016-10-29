
#setwd("C:/Users/Alex/Dropbox/job search/Data incubator/2nd application/Q3/lib")

library(shiny)
library(leaflet)
library(data.table)
library(dplyr)
load('../output/grid.RData')

d = round(mean(diff(grid$lat)[diff(grid$lat)>0]), 3) # read pixel size



ui <- bootstrapPage(
  #tags$head(includeCSS("../doc/styles.css")),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(class = "panel panel-default", top = 10, right = 10,
                h4("Select preferences"),
                sliderInput("w_subway", "   Transportation:", min=1, max=100, value=50),
                sliderInput("w_schools", label = "   Schools:", min=1, max=100, value=50),
                sliderInput("opac", label = "   Opacity:", min=0, max=1, value=0.5),
                submitButton("Update")
  )
)




server <- function(input, output, session) {
  
  # initialize map:
  map = leaflet() %>%
    addTiles() %>%
    setView(lng = -73.87, lat = 40.75, zoom = 11)
  

  #draw map:
  color_scale = colorRamp(c("blue", "cyan", "green", "yellow", "orange" ,"red"))

  observe({
    
    score = input$w_schools*grid$school + input$w_subway*grid$subway
    score = (score - min(score, na.rm=T)) / (max(score, na.rm=T) - min(score, na.rm=T))
    score = ifelse(!is.na(score), score, 0)
    
    leafletProxy("map") %>% clearShapes() %>%
      addRectangles(lng1 = grid$lon-d/2, lat1 = grid$lat-d/2,
                    lng2 = grid$lon+d/2, lat2 = grid$lat+d/2,
                    fillColor=rgb(color_scale(score)/255), 
                    fillOpacity=input$opac, 
                    weight=0)
    
      
  })
  
  # render map:
  output$map = renderLeaflet({
    map
  })
  
  
}

shinyApp(ui, server)






