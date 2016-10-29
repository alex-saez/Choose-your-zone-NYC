

get_polygon_points = function(the_geom){
  # Extract points necessary for plotting polygon on map
  # Input: 'the_geom' field of a NYC data table
  
  library(dplyr)
  
  the_geom = gsub("MULTIPOLYGON \\(", "", the_geom)
  the_geom = gsub("\\(", "", the_geom)
  the_geom = gsub("\\)", "", the_geom)
  
  points = the_geom %>% 
              strsplit(', ') %>%
              unlist() %>%
              strsplit(' ') %>%
              unlist() %>%
              as.numeric()
    
  point_coords = data.frame(lon = points[seq(1, length(points), 2)], 
                            lat = points[seq(2, length(points), 2)])

  return(point_coords)

}








