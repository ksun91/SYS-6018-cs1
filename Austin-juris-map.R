require(gdata)
library(ks)
library(RColorBrewer)
require(rgdal)
library(MASS)

setwd("~/Documents/DSI/notes/2-SYS-6018/Case Study 1 - crime")
source("load-Austin-data.R")

# LOAD THE DATA
boundary <-readOGR(dsn="Neighborhood-Planning-Areas",layer="geo_export_bf249074-a2fc-4d35-b70e-d75de1175a06")
plot(boundary)

stores <- read.csv("stores_info.csv", stringsAsFactors = F)
plot(stores$Long, stores$Lat)
#figure out how to plot this on top of boundary

crimes <- read.csv("Annual_Crime_Dataset_2015.csv", stringsAsFactors = F)
# convert GO_X and GO_Y to lat and long, then plot on top of boundary


# get color palette for heat map
palette.function = colorRampPalette(rev(brewer.pal(11,'Spectral')))
heat.colors = palette.function(32)


# EXPLORATORY PLOT THE DATA TO SEE WHAT WE'RE WORKING WITH
plot(stores, pch = 20, col=stores$store)
plot(boundary, add = TRUE)
title("Cultural Hot Spots in Toronto")
# legend("topright", legend = levels(td$CATEGORY), 
#        col = td$CATEGORY) # can't get this to work for some reason

### OK, now do it for real

# # seems like this should work, but it doesn't...
# tdKDE1 = kde2d(td$LATITUDE, td$LONGITUDE, h=0.01, n=300)
# image(tdKDE1, col = heat.colors, useRaster=TRUE, asp=1)
# plot(boundary, add = TRUE)
# points(td$LATITUDE, td$LONGITUDE, pch = 20, col="grey")
# # so instead... we turn it into a data.frame?

## subset out just the lat and long 
tddf <- as.data.frame(td)
td.sample.points <- tddf[ ,(ncol(tddf)-1):ncol(tddf)]
names(td.sample.points) <- c("lat", "long")

## NOW PLOT
td1 = kde2d(td.sample.points$lat, td.sample.points$long, 
             h=1, n=300, lims = tlims)
var(c(td1$z))
image(td1, col = heat.colors, useRaster=TRUE, asp=1)
plot(boundary, add = TRUE)
# h way too high

td.1 = kde2d(td.sample.points$lat, td.sample.points$long, 
             h=0.1, n=300, lims = tlims)
var(c(td.1$z))
image(td.1, col = heat.colors, useRaster=TRUE, asp=1)
plot(boundary, add = TRUE)
# h pretty good, but a little rough

td.01 = kde2d(td.sample.points$lat, td.sample.points$long, 
             h=0.01, n=300, lims = tlims)
var(c(td.01$z))
image(td.01, col = heat.colors, useRaster=TRUE, asp=1)
plot(boundary, add = TRUE)
# h too fine

td.05 = kde2d(td.sample.points$lat, td.sample.points$long, 
             h=0.05, n=300, lims = tlims)
var(c(td.05$z))
image(td.05, col = heat.colors, useRaster=TRUE, asp=1)
plot(boundary, add = TRUE)
# goldilocks. That's a pretty good and descriptive fit.
# the variance of the output is high, but not too high.

#now plot the points in the sample data to compare
points(td.sample.points$lat, td.sample.points$long, pch = 20, col="grey")
# hard to say, this might be overfit, or it might be perfect
##probably depends on your application

# we're excited to have found this manually, but now we see what
# Silverman's Rule of Thumb method gives us:
bandwidth.nrd(td.sample.points$lat)
#[1] 0.17677
bandwidth.nrd(td.sample.points$long)
#[1] 0.07473002
##so we were pretty close on the longitude, but only semi-close on latitude

# here is what it looks like with the "optimal" h values
tdn = kde2d(td.sample.points$lat, td.sample.points$long, 
              h=c(bandwidth.nrd(td.sample.points$lat), 
                  bandwidth.nrd(td.sample.points$long)),
              n=300, lims = tlims)
image(tdn, col = heat.colors, useRaster=TRUE, asp=1)
plot(boundary, add = TRUE)
#and with points
points(td.sample.points$lat, td.sample.points$long, pch = 20, col="grey")
