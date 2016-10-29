
# load table containg the longitide and latitude of each subway station, found at: 
# (https://data.cityofnewyork.us/Transportation/Subway-Stations/arq3-7z49), 
# assign "weight" of each station according to how many lines pass by it,
# then use ggmap function revgeocode to get zipcode from longitide and latitude coordinates
# then save new table with weights and zipcodes for each station into the 'output' folder
# also create table with the sum of sation weights divided by the surface area of  each zipcode 
# and save to 'output' folder

library(dplyr)
library(ggmap)

subway = read.csv('../data/SUBWAY_STATIONS.csv', as.is = TRUE)

subway = subway %>%
          select(-URL) %>%  
          filter(!grepl('[0-9]-[A-Z][a-z]', subway$LINE) & !grepl('/', subway$LINE)) # remove rows where LINE contains a date


# assign weight to each station according to how many subway lines go by
subway$weight = sapply(subway$LINE, function(x){return(length(strsplit(x, '-')[[1]]))})

# functions to extract longitude and latitude from 'the_geom' column
get_lon = function(x){
  lon = substr(x, 8, nchar(x)-1)
  lon = strsplit(lon, ' ')[[1]][1]
  lon = as.numeric(lon)
  return(lon)
}
get_lat = function(x){
  lat = substr(x, 8, nchar(x)-1)
  lat = strsplit(lat, ' ')[[1]][2]
  lat = as.numeric(lat)
  return(lat)
}

# add lon and lat columns to table:
subway$lon = sapply(subway$the_geom, get_lon)
subway$lat = sapply(subway$the_geom, get_lat)

subway = select(subway, -the_geom)

# use ggmap function revgeocode to get zipcode from coordinates and save into new column
for(i in 1:nrow(subway)){
  lonlat = as.numeric(subway[i, c('lon','lat')])
  loc = revgeocode(lonlat, output = "more")
  subway$zipcode[i] = as.integer(as.character(loc$postal_code))
}

# replace obsolete zipcode:
subway$zipcode[subway$zipcode==11381] = 11379


write.csv(subway, "../output/subway_locations.csv", row.names=FALSE)

# subway = read.csv("../output/subway_locations.csv", as.is = TRUE)

# compute wheighed sum of subway stations per zipcode
subway_dens_per_zip = subway %>%
  group_by(zipcode) %>%
  summarise(tot_weight = sum(weight))

# load table with surface area for each zipcode and add area to subway_dens_per_zip:
zip_surface = read.csv("../data/zipcode surface areas.csv", as.is = TRUE)
subway_dens_per_zip = subway_dens_per_zip %>%
                      left_join(zip_surface, by=c("zipcode" = "ZCTA")) %>%
                      mutate(density = tot_weight/ZAREALAND) %>%
                      mutate(density = 100*order(density)/length(density)) # normalize to 0-100


write.csv(subway_dens_per_zip, "../output/subway_dens_per_zip.csv", row.names=FALSE)

# library(devtools)
# install_github('arilamstein/choroplethrZip@v1.4.0')

library(choroplethrZip)
dd = subway_dens_per_zip %>%
      mutate(region=as.character(zipcode), value=density) %>%
      select(region, value)
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
zip_choropleth(dd, county_zoom=nyc_fips, num_colors=9,legend="Density")

