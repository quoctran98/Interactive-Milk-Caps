library(ggplot2)

stateAbbre <- read.csv("otherData/abbreviations.csv", header=TRUE)
stateAbbre$State <- toupper(stateAbbre$State)
load("./rawData/zipcode.rda") # from https://cran.r-project.org/src/contrib/Archive/zipcode/
source("functions.R")
source("rawData/dataCleanup.R")

standardGraph <- function(stateFilt = NA) {
  
  trimmedResponses <- merge(responses,zipcode, by="zip")
  
  if (!is.na(stateFilt)) {
    stateFilt <- tolower(stateFilt)
    trimmedResponses <- subset(trimmedResponses, trimmedResponses$state %in% as.character(stateAbbre[stateAbbre$State %in% toupper(stateFilt), "Code"]))
  }
  
  allColors <- unique(c(as.character(trimmedResponses$wholeHex), 
                        as.character(trimmedResponses$reducedHex), 
                        as.character(trimmedResponses$lowHex), 
                        as.character(trimmedResponses$skimHex)))
  allColors <- allColors[!is.na(allColors)]
  
  colFatDF <- data.frame(matrix(ncol = 3))
  colnames(colFatDF) <- c("color", "milkfat", "num")
  
  for (col in allColors) {
    for (mf in c("Whole", "Reduced", "Low", "Skim")) {
      milkfatHex <- paste(tolower(mf),"Hex",sep = "")
      colFatDF[nrow(colFatDF) + 1, "color"] <- col
      colFatDF[nrow(colFatDF), "milkfat"] <- mf
      colFatDF[nrow(colFatDF), "num"] <- sum(col == trimmedResponses[,milkfatHex], na.rm = TRUE) / (length(trimmedResponses[,milkfatHex]) - sum(is.na(trimmedResponses[,milkfatHex])))
    }
  }
  
  colFatDF <- colFatDF[-1,]

  # Refactoring so everything's in the right order
  colFatDF$color <- factor(colFatDF$color, levels = allColors)
  colFatDF$milkfat <- factor(colFatDF$milkfat, levels = c("Whole","Reduced","Low","Skim"))
  
  ggplot(colFatDF, aes(x = milkfat, y = num)) +
    geom_col(aes(fill = color), color = "black") +
    scale_fill_manual(values = allColors) + 
    theme(legend.position="none") +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=30))
    
}