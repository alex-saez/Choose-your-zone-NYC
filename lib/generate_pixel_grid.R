
# setwd("C:/Users/Alex/Dropbox/job search/Data incubator/2nd application/Q3/lib")
setwd("~/Dropbox/job search/Data incubator/2nd application/Q3/lib")

library(data.table)
library(dplyr)
library(leaflet)
library(mgcv)
zones = fread('../data/school_zones.csv')


A = readRDS("../data/USA_adm2.rds")
county = A[A$NAME_1=='New York' & A$NAME_2=='New York',]
manh = county@polygons[[1]]@Polygons[[1]]@coords
county = A[A$NAME_1=='New York' & A$NAME_2=='Kings',]
bkly = county@polygons[[1]]@Polygons[[1]]@coords
county = A[A$NAME_1=='New York' & A$NAME_2=='Queens',]
qns = county@polygons[[1]]@Polygons[[1]]@coords
county = A[A$NAME_1=='New York' & A$NAME_2=='Bronx',]
brnx = county@polygons[[1]]@Polygons[[1]]@coords
county = A[A$NAME_1=='New York' & A$NAME_2=='Richmond',]
stis = county@polygons[[1]]@Polygons[[1]]@coords


d = 0.002
lons = seq(-74.25584, -73.704193, d)
lats = seq(40.49617, 40.920101, d)
grid = expand.grid(lon=lons, lat=lats)
grid = as.matrix(grid)


ind_nyc = in.out(manh, grid) | in.out(bkly, grid) | in.out(qns, grid) | in.out(brnx, grid) | in.out(stis, grid)
grid = grid[ind_nyc,] # select nyc points only
grid = as.data.frame(grid)




save(grid, file = '../output/grid_002.RData')

colors = runif(nrow(grid))
leaflet() %>% addTiles() %>%
  addRectangles(lng1 = grid$lon-d/2, lat1 = grid$lat-d/2,
                lng2 = grid$lon+d/2, lat2 = grid$lat+d/2,
                fillColor=gray(colors), fillOpacity=.3, weight=0)


# map = leaflet() %>%  
#   addTiles() %>%
#   addPolygons(lng = manh[,1],
#               lat = manh[,2],
#               fillColor= 'red',
#               fillOpacity=0.5)
# map
# 
