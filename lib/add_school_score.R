
# Code for adding a school score to each NYC pixel based on the specified column 
# of data table loaded as 'score_data'. 

library(data.table)
library(dplyr)
library(mgcv)
source('../lib/get_polygon_points.R')

load('../output/grid.RData')

zones = fread('../data/ES_Zones_2016-2017.csv', verbose=FALSE)
zones = zones %>% filter(ZONED_DIST != 0) 

score_data = fread('../data/School_Progress_Report_2010-2011.csv')
score_data = score_data %>% select(DBN, SCORE=`2010-2011 OVERALL SCORE`)



#################### get score from score_data and add it to zones ####################################
get_score = function(x){
  x = gsub('\ ','',x)
  ind = which(score_data$DBN %in% unlist(strsplit(x,',')))
  return(mean(score_data$SCORE[ind]))
}

zone_scores = sapply(zones$DBN, get_score)

zones = zones %>% 
            select(ESID_NO, DBN, Label, the_geom, Shape_Area) %>% 
            mutate(SCORE = zone_scores) %>%
            filter(the_geom != 'MULTIPOLYGON EMPTY')

# write.csv(zones, file = '../data/school_zones.csv', row.names = FALSE)

#################### get score from score_data and add it to zones ####################################

zones = fread('../data/school_zones.csv')
grid = as.matrix(grid)

score = numeric(nrow(zones))
for(i in 1:nrow(zones)){
  coords = as.matrix(get_polygon_points(zones$the_geom[i]))
  ind_zone = in.out(coords, grid) 
  score[ind_zone] = zones$SCORE[i]
}

score = 100*(score - min(score, na.rm=T)) / (max(score, na.rm=T) - min(score, na.rm=T))

grid = data.frame(grid, school = score)

save(grid, file = '../output/grid.RData')


############ for checking zones #####################################################
colors = colorRamp(c("black", "red"))
score = ifelse(!is.na(score), score, 0)


regions = zones#[zones$DBN=='05M036,05M125',]
m = leaflet() %>% addTiles()
for(i in 1:nrow(regions)){
  poly_points = get_polygon_points(regions$the_geom[i])
  m = addPolygons(m, lng = poly_points$lon, lat = poly_points$lat,
                  fillColor= rgb(colors(score[i]/100)/255),
                  fillOpacity=.5,
                  weight=1)
}
m

