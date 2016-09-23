require(gdata)
library(ks)
library(RColorBrewer)
require(rgdal)
library(MASS)
require(dplyr)

source("CrimeUtil.R")

#setwd("~/Documents/DSI/notes/2-SYS-6018/Case Study 1 - crime/SYS-6018-cs1")

# LOAD THE DATA
# boundary <-readOGR(dsn="Neighborhood-Planning-Areas",layer="geo_export_bf249074-a2fc-4d35-b70e-d75de1175a06")
# plot(boundary)

boundary <-readOGR(dsn=".", layer="geo_export_7c52f690-58f6-4f4a-96fa-95df45d3770a")

zips <-readOGR(dsn="Zipcodes", layer="geo_export_314dd5e2-dc9d-4a81-b190-c1dd214574c4")

# This will draw the entire map of Austin. Available for export from: 
# https://data.austintexas.gov/Government/Austin-Police-Sectors-and-Districts/bh6h-vpxb
#boundary <-readOGR(dsn="Austin Police Sectors and Districts", layer="geo_export_7c52f690-58f6-4f4a-96fa-95df45d3770a")

plot(boundary)
# plot(zips)

stores <- read.csv("stores_info.csv", stringsAsFactors = F)
store_coords <- data.frame(x=stores$Long, y=stores$Lat)
coordinates(store_coords) <- c('x','y')
plot(store_coords, col="blue", add=T)

#crimes <- read.csv("Annual_Crime_Dataset_2015.csv", stringsAsFactors = F)
crimes <- read.csv("crimesNotStores.csv", stringsAsFactors = F)
crimes <- filter(crimes, GO.X.Coordinate != "") # remove crimes that have no location data

coords <- data.frame(x=crimes$GO.X.Coordinate, y=crimes$GO.Y.Coordinate)
coordinates(coords) <- c('x','y')
proj4string(coords) <- CRS("+init=epsg:2277")
coords_r <- spTransform(coords,CRS("+init=epsg:4326"))

crimes$long <- coordinates(coords_r)[,1]
crimes$lat <- coordinates(coords_r)[,2]

# Reproject crime lon/lat points to meters
crime.locations.lonlat = cbind(crimes$long, crimes$lat)
crime.locations.meters = project(crime.locations.lonlat, proj="+init=epsg:26971")

# Reproject store location points to meters
store.locations.lonlat = cbind(stores$Long, stores$Lat)
store.locations.meters = project(store.locations.lonlat, proj="+init=epsg:26971")

#####
# Reproject store location (BUT TRY TO KEEP THE OTHER INFO)
store.locations.lonlat = cbind(stores$Long, stores$Lat)
store.locations.meters = project(store.locations.lonlat, proj="+init=epsg:26971")
storesFULL <- cbind(stores, store.locations.meters)
storesFULL$store <- as.factor(storesFULL$store)
storesFULL$walORnot <- ifelse(storesFULL$store=="Walmart", 19, 18)
storesFULL$pointsize <- ifelse(storesFULL$store=="Walmart", 2, 1)
coordinates(storesFULL) <- c('1','2')
#####

# Add distance to closest bus stops in meters from each store location
# NOTE: This code needs to be run after 'Load-Austin-data.R'

# Source for MetroBus lines/MetroRail stops: 
# https://data.texas.gov/Capital-Metro/GTFS/r4v4-vz24
st <- read.table("stops.txt", sep=",", header=T)
stops <- as.matrix(select(st, stop_lon, stop_lat))
stops.meters = project(stops, proj="+init=epsg:26971")

# Find the shortest distance in meters to the closest bus stop from
# each store address
closest.stops <- data.frame(get.min.distances(store.locations.meters,stops.meters))
names(closest.stops) <- 'closest_stops_in_meters'
stores_stops <- cbind(stores, closest.stops)
stores_stops <- select(stores_stops, address, `closest_stops_in_meters`)
write.csv(stores_stops, "stores_stops.csv", row.names=F)

# Create data frame from crime.locations.meters
crime.locations <- as.data.frame(crime.locations.meters)
names(crime.locations) <- c("x","y")

kde.sample.points = crime.locations[,c("x","y")]

austin = spTransform(boundary, CRS(proj="+init=epsg:26971"))

zips_a = spTransform(zips, CRS(proj="+init=epsg:26971"))

# get estimation points for KDE
kde.resolution.meters = 100
kde.est.points = get.grid.points(austin, kde.resolution.meters, FALSE)

# run KDE, using 500 points from the sample
kde.est = run.spatial.kde(kde.sample.points, kde.est.points, 500) 

##### PLOT
# plot KDE of crimes
plot.spatial.kde(kde.est, kde.est.points)

# plot city lines
plot(austin, add=T) # police jurisdictions and city limits
#plot(zips_a, add=T) # zip codes

# plot stores
points(storesFULL, 
       col= storesFULL$store, 
       #col = "white",
       pch = storesFULL$walORnot, 
       cex = storesFULL$pointsize)
######
