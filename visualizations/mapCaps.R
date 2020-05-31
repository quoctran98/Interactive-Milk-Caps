library(ggplot2)
library(mapdata)
library(ggmap)

states <- map_data("state")
stateAbbre <- read.csv("otherData/abbreviations.csv", header=TRUE)
stateAbbre$State <- toupper(stateAbbre$State)
load("./rawData/zipcode.rda") # from https://cran.r-project.org/src/contrib/Archive/zipcode/
source("functions.R")
source("rawData/dataCleanup.R")

mapCaps <- function(milkfat, brandFilt=NA, stateFilt=NA) {
  # Trimming and merging raw data
  trimmedResponses <- merge(responses,zipcode, by="zip")
  trimmedStates <- states
  
  trimmedResponses <<- trimmedResponses
  
  # Filtering the dataset by brandFilt
  if (!is.na(brandFilt)) {
    trimmedResponses <- subset(trimmedResponses, brand %in% brandFilt)
  }
  
  # Filtering map data and dataset by statFilT
  if (!is.na(stateFilt)) {
    stateFilt <- tolower(stateFilt)
    trimmedStates <- subset(states, region %in% stateFilt)
    trimmedResponses <- subset(trimmedResponses, trimmedResponses$state %in% as.character(stateAbbre[stateAbbre$State %in% toupper(stateFilt), "Code"]))
  }

  # Plotting the points on a map
  milkfatHex <- paste(milkfat,"Hex",sep = "")
  ggplot(data=trimmedStates) + 
    geom_polygon(aes(x=long, y=lat, group=group), fill = "gray50", color = "gray35", size = 2) + 
    geom_point(data=trimmedResponses, aes(x=longitude, y=latitude), color=trimmedResponses[,milkfatHex], size=2.5) +
    coord_fixed(1.3) +
    guides(fill=FALSE) +
    theme_nothing() +
    theme(plot.background = element_rect(fill = "white"))
}
