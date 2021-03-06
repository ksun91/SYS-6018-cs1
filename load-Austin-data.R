#setwd("~/Documents/DSI/notes/2-SYS-6018/Case Study 1 - crime/SYS-6018-cs1")

require(gdata)
library(MASS)
library(dplyr)
require(ks)
source("CrimeUtil.R")

# LOAD THE DATA
stores <- read.csv("data/stores_info.csv", stringsAsFactors = F)

crimes <- read.csv("data/Annual_Crime_Dataset_2015.csv", stringsAsFactors = F)
crimes <- filter(crimes, GO.X.Coordinate != "") # remove crimes that have no location data

####

getCrimes <- function(add, crimes) {
  # if there's a comma in the address, strip off what's before the comma (shopping mall name, etc.)
  if (grepl(",", add)) {
    add <- gsub("^.+, ", "", add)
  }
  # split on blank space
  addS <- unlist(strsplit(add, " "))
  # find the addresses that match
  winners <- sapply(crimes$GO.Location, addyPart, addS = addS, USE.NAMES = F)
  # subset to only those addresses
  addDF <- crimes[winners, ]
  # print the count
  print(paste(add, nrow(addDF))) 
  # return the subset
  addDF
}

addyPart <- function(addToCheck, addS) {
  # set the threshold for what percentage of the address has to match
  thresh <- .6 * length(addS)
  # initiate the count of matching words
  count <- 0
  # make both lower case
  addS <- tolower(addS)
  addToCheck <- tolower(addToCheck)
  # store the house number (or if there isn't one, store FALSE)
  if (grepl('[0-9]', addS[1])) {
    num <- addS[1]
  } else {
    num <- FALSE
  }
  # for each word in the address, store the count of how many are in the address we're checking
  for (i in 1:length(addS)){
    if (grepl(paste('\\b',addS[i],'\\b',sep=''), addToCheck)) {
      count <- count + 1
    }
  }
  if (count >= thresh) {
    # if we pass the threshold AND it matches the house number, return TRUE
    if (num != FALSE & grepl(paste('\\b',num,'\\b',sep=''), addToCheck)) {
      TRUE
    } else {
      FALSE
    }
  } else {
    FALSE
  }
}

# make a list with a data frame for each store and all its crimes
scdf <- lapply(stores$address, getCrimes, crimes = crimes)

#make a data frame with counts for each address
storeCrimeCount <- data.frame(store = character(nrow(stores)),
                              address = character(nrow(stores)),
                              crimes = numeric(nrow(stores)), stringsAsFactors = F)
for (j in 1:nrow(stores)) {
  storeCrimeCount[j, ] <- c(stores$store[j], stores$address[j], nrow(scdf[[j]]))
}

# look at the addresses with zero crimes
zeros <- storeCrimeCount[storeCrimeCount$crimes==0, ]
# what percentage of our data set is zero?
nrow(zeros) / nrow(stores)

# create data.frame with store info and total crimes count ## OR load it below from a .csv
stores <- cbind(stores, as.numeric(storeCrimeCount$crimes))
names(stores)[ncol(stores)] <- "crimes2015"
head(stores)


##### bring in economic data
econ <- read.csv("data/socio_econ.csv", header=T, stringsAsFactors = F)
master <- merge(stores, econ, by.x = 'zip', by.y = 'Zipcode')
names(master)[6:7] <- c("Long", "Lat")
#####

##### bring in KDE data
# subset out all crimes NOT at the stores we're analyzing
storesIndex <- numeric()
for (i in 1:length(scdf)){
  storesIndex <- c(storesIndex, row.names(scdf[[i]]))
}

storesIndex <- as.numeric(storesIndex)
crimesNotStores <- crimes[-storesIndex, ]
#write.csv(crimesNotStores, "data/crimesNotStores.csv", row.names=F)

# # subset out crimes AT the stores
# crimesStores <- crimes[storesIndex, ]
# crimesAtStores <- as.data.frame(table(crimesStores$GO.Highest.Offense.Desc))

# remove crimes that have no location data
CNS <- filter(crimesNotStores, GO.X.Coordinate != "") 

# reproject crimes data for KDE purposes
coords <- data.frame(x=CNS$GO.X.Coordinate, y=CNS$GO.Y.Coordinate)
coordinates(coords) <- c('x','y')
proj4string(coords) <- CRS("+init=epsg:2277")
coords_r <- spTransform(coords,CRS("+init=epsg:4326"))
# set as coordinates
CNS$long <- coordinates(coords_r)[,1]
CNS$lat <- coordinates(coords_r)[,2]
# Reproject crime lon/lat points to meters
crime.locations.lonlat = cbind(CNS$long, CNS$lat)
crime.locations.meters = project(crime.locations.lonlat, proj="+init=epsg:26971")
# Create data frame from crime.locations.meters
crime.locations <- as.data.frame(crime.locations.meters)
names(crime.locations) <- c("x","y")
kde.sample.points = crime.locations[,c("x","y")]

# Reproject store points for KDE purposes
# store_coords <- data.frame(x=stores$Long, y=stores$Lat)
# coordinates(store_coords) <- c('x','y')
# Reproject store location points to meters
store.locations.lonlat = cbind(stores$Long, stores$Lat)
store.locations.meters = project(store.locations.lonlat, proj="+init=epsg:26971")
# Create data frame from store.locations.meters
store.locations <- as.data.frame(store.locations.meters)
names(store.locations) <- c("x","y")
store.points = store.locations[,c("x","y")]
# get KDE prediction for store points and save as .csv
kde.stores.est = run.spatial.kde(kde.sample.points, store.points, 5000) # THIS TAKE A MINUTE (could decrease the sample size if we wanted it run quicker)
storesKDE <- cbind(stores[,1:2], kde.stores.est)
names(storesKDE)[3] <- "KDEraw"
#write.csv(storesKDE, "data/storesKDE5000.csv", row.names = F)

# merge KDE with master
master <- merge(master, storesKDE, by = c("store", "address"))

##### bring in bus stop data
bus <- read.csv("data/stores_stops.csv", header=T, stringsAsFactors = F)
master <- merge(master, bus, by = "address")

##### bring in store hours data
hours <- read.xls("data/store_hours.xlsx", header=T, sheet = 1)
master <- merge(master, hours, by = c("store", "address"))


##### subset and save master
master <- subset(master, 
                 select = -c(X, Lat.y, Long.y, City, State,
                    average.SAT.score, High.school.drop., TotStud))
#write.csv(master, "MASTER_DATA.csv", row.names=F)
