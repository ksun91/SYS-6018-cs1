require(gdata)
library(ks)
library(RColorBrewer)
require(rgdal)
library(MASS)

setwd("~/Documents/DSI/notes/2-SYS-6018/Case Study 1 - crime")

# LOAD THE DATA
boundary <-readOGR(dsn="Neighborhood-Planning-Areas",layer="geo_export_bf249074-a2fc-4d35-b70e-d75de1175a06")
plot(boundary)

stores <- read.csv("stores_info.csv", stringsAsFactors = F)
plot(stores$Long, stores$Lat)
store_coords <- data.frame(x=stores$Long, y=stores$Lat)
coordinates(store_coords) <- c('x','y')
plot(store_coords, col="red", add=T)

crimes <- read.csv("Annual_Crime_Dataset_2015.csv", stringsAsFactors = F)
# convert GO_X and GO_Y to lat and long, then plot on top of boundary

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

### FAKE UNIT TEST
#addyPart("Ben White 710", c("710", "E", "Ben", "White", "Blvd")) # should be TRUE
#addyPart("Ben White", c("710", "E", "Ben", "White", "Blvd")) # should be FALSE
#bw <- getCrimes("710 E Ben White Blvd", crimes)

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

