
# setwd("C:/Users/Alex/Dropbox/job search/Data incubator/2nd application/Q3/lib")
setwd("~/Dropbox/job search/Data incubator/2nd application/Q3/lib")

library(data.table)
library(dplyr)
library(leaflet)
library(mgcv)
source('../lib/get_polygon_points.R')


# generate broad grid:
d = 0.002
lons = seq(-74.25584, -73.704193, d)
lats = seq(40.49617, 40.920101, d)
grid = expand.grid(lon=lons, lat=lats)
grid = as.matrix(grid)


# remove extra pixels using school-zone contours for filtering:
zones = fread('../data/ES_Zones_2016-2017.csv', verbose=FALSE)
park_esid_nos = c(533,739,605,609,600,402,442,321,325,326,146,99,208,178,168,202,531,203)
zones = zones %>% 
  filter(ZONED_DIST != 0) %>% 
  filter(the_geom != 'MULTIPOLYGON EMPTY') %>% 
  filter(!ESID_NO %in% park_esid_nos) %>% 
  select(ESID_NO, DBN, Label, the_geom, Shape_Area)

# write.csv(zones, file = '../data/school_zones.csv', row.names = FALSE)


good_ind = integer()
for(i in 1:nrow(zones)){
  coords = as.matrix(get_polygon_points(zones$the_geom[i]))
  #good_ind = c(good_ind, which(in.out(coords, grid)))
  good_ind = c(good_ind, which(in.out(coords, grid_lonlat)))
}

grid = grid[good_ind,]

save(grid, file = '../output/grid_002.RData')



#### for checking: ###############################################
colors = runif(nrow(grid))
leaflet() %>% addTiles() %>%
  addRectangles(lng1 = grid$lon-d/2, lat1 = grid$lat-d/2,
                lng2 = grid$lon+d/2, lat2 = grid$lat+d/2,
                fillColor= 'red', #gray(colors), 
                fillOpacity=.5, 
                weight=0)

