library(zipcode)

stateAbbre <- read.csv("otherData/abbreviations.csv", header=TRUE)
stateAbbre$State <- toupper(stateAbbre$State)
data("zipcode")
source("functions.R")
source("rawData/dataCleanup.R")

cleanedBrands <- read.csv("brands/consensusEdited.csv") # Manually cleaned brands

getBrands <- function(stateFilt = NA) {
  
  trimmedResponses <- merge(responses,zipcode, by="zip")
  
  if (!is.na(stateFilt)) {
    stateFilt <- tolower(stateFilt)
    trimmedResponses <- subset(trimmedResponses, trimmedResponses$state %in% as.character(stateAbbre[stateAbbre$State %in% toupper(stateFilt), "Code"]))
  }
  
  brandRaw <- as.character(unique(trimmedResponses$brand)) 
  # Brand raw will be more comprehensive than brand stylized (brand stylization is curated!)
  brandStyleRow <- match(brandRaw, cleanedBrands$brand)
  return(list("raw" = brandRaw,
              "stylized" = cleanedBrands[brandStyleRow[!is.na(brandStyleRow)], "stylization"]))

}