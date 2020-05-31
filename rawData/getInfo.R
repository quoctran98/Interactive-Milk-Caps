library(ggplot2)

stateAbbre <- read.csv("otherData/abbreviations.csv", header=TRUE)
stateAbbre$State <- toupper(stateAbbre$State)
load("./rawData/zipcode.rda") # from https://cran.r-project.org/src/contrib/Archive/zipcode/
source("functions.R")
source("rawData/dataCleanup.R")

getInfo <- function(stateFilt = NA) {
  
  trimmedResponses <- merge(responses,zipcode, by="zip")
  
  if (!is.na(stateFilt)) {
    stateFilt <- tolower(stateFilt)
    trimmedResponses <- subset(trimmedResponses, trimmedResponses$state %in% as.character(stateAbbre[stateAbbre$State %in% toupper(stateFilt), "Code"]))
  }
  
  numResponses <- nrow(trimmedResponses)
  
  return(
    list("numResponses" = numResponses)
  )
}