library(ggplot2)
library(reshape2)
cleanedBrands <- read.csv("brands/consensusEdited.csv") # Manually cleaned brands
colorReference <- read.csv("otherData/colorReference.csv", header=FALSE)

brandChart <- function(rawBrand) {
  
  # Trimming the brands to only the ones I have in the edited spreadsheet
  trimmedBrands <- cleanedBrands[cleanedBrands$brand %in% rawBrand,]
  
  # Merging colors
  colnames(colorReference) <- c("whole","wholeHex")
  trimmedBrands <- merge(trimmedBrands,colorReference, by="whole", all.x=TRUE)
  colnames(colorReference) <- c("reduced","reducedHex")
  trimmedBrands <- merge(trimmedBrands,colorReference, by="reduced", all.x=TRUE)
  colnames(colorReference) <- c("low","lowHex")
  trimmedBrands <- merge(trimmedBrands,colorReference, by="low", all.x=TRUE)
  colnames(colorReference) <- c("skim","skimHex")
  trimmedBrands <- merge(trimmedBrands,colorReference, by="skim", all.x=TRUE)
  colnames(colorReference) <- c("color","hex")
  
  # Making and populating the dataframe for the "heatmap"
  df <- data.frame(matrix(ncol = 3))
  colnames(df) <- c("brand", "milkfat", "color")
  
  for (brand in trimmedBrands$brand) {
    for (mf in c("Whole", "Reduced", "Low", "Skim")) {
      milkfatHex <- paste(tolower(mf),"Hex",sep = "")
      df[nrow(df) + 1, "brand"] <- as.character(trimmedBrands[trimmedBrands$brand == brand, "stylization"])
      df[nrow(df), "milkfat"] <- mf
      df[nrow(df), "color"] <- as.character(trimmedBrands[trimmedBrands$brand == brand, milkfatHex])
    }
  }
  df <- df[-1,]
  
  # Refactoring so everything's in the right order
  allColors <- as.character(unique(df$color))
  df$color <- factor(df$color, levels = allColors)
  df$milkfat <- factor(df$milkfat, levels = c("Whole","Reduced","Low","Skim"))
  
  ggplot(df, aes(x = milkfat, y = brand)) + 
    geom_tile(aes(fill = color), size = 1, color = "black") +
    scale_fill_manual(values = allColors) + 
    theme(legend.position="none") +
    theme(axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.x=element_blank(),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=15))
  
}