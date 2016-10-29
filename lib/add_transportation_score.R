

library(data.table)
library(dplyr)

load('../output/grid.RData')

subway = fread('../data/subway_locations.csv')
subway  = subway %>% select(lon, lat, weight)

score_pixel = function(point){
  sigma = 0.005
  dists = sqrt((subway$lon - point[1])^2 + (subway$lat - point[2])^2)
  scores = subway$weight*exp(-(dists/sigma)^2/2)
  scores[scores<0] = 0
  return(sum(scores)) 
}

score = apply(as.matrix(grid[,1:2]), 1, score_pixel)
score = 100*(score - min(score, na.rm=T)) / (max(score, na.rm=T) - min(score, na.rm=T))

grid = data.frame(grid, subway = score)

save(grid, file = '../output/grid.RData')



########### for testing: ###################################################

color_scale = colorRamp(c("blue", "cyan", "green", "yellow", "orange" ,"red"))

d = round(mean(diff(grid$lat)[diff(grid$lat)>0]), 3) # read pixel size

score = transportation
score = (score - min(score, na.rm=T)) / (max(score, na.rm=T) - min(score, na.rm=T))
score = ifelse(!is.na(score), score, 0)


leaflet() %>% addTiles() %>%
  addRectangles(lng1 = grid$lon-d/2, lat1 = grid$lat-d/2,
                lng2 = grid$lon+d/2, lat2 = grid$lat+d/2,
                fillColor=rgb(color_scale(score)/255), 
                fillOpacity=.5, 
                weight=0)
