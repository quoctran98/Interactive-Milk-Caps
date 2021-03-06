library(shiny)
library(tidyverse)

source("visualizations/mapCaps.R")
source("visualizations/standardGraph.R")
source("visualizations/standardPie.R")
source("rawData/getInfo.R")
source("brands/getBrands.R")
source("visualizations/brandChart.R")

allStates <- read.csv("otherData/abbreviations.csv")$State
allStates <- allStates[allStates != "Hawaii"]
allStates <- allStates[allStates != "Alaska"]
allStates <- allStates[allStates != "District of Columbia"]

function (input, output, session) {
  
  observe({
    updateSelectInput(session, "stateSelect", selected = input$stateSelectLess)
  })
  
  observe({
    #updateSelectInput(session, "stateSelect", 
          #            selected = ifelse(input$regionSelect == "United States", 
          #                              as.character(allStates),
           #                             input$stateSelect))
  })
  
  output$blurbState <- renderText({
    info <- getInfo(input$stateSelect)
    paste("We have ",
          info$numResponses,
          ifelse(info$numResponses == 1, " response from ", " responses from "),
          wordsList(input$stateSelect), 
          "!",
          sep = "")
  })
  
  output$mapCapsState <- renderPlot({
    mapCaps(tolower(input$milkfatSelect), stateFilt = input$stateSelect)
  })
  
  output$standardGraphStateWhole <- renderPlot({
    standardPie("Whole", stateFilt = input$stateSelect)
  })
  output$standardGraphStateReduced <- renderPlot({
    standardPie("Reduced", stateFilt = input$stateSelect)
  })
  output$standardGraphStateLow <- renderPlot({
    standardPie("Low", stateFilt = input$stateSelect)
  })
  output$standardGraphStateSkim <- renderPlot({
    standardPie("Skim", stateFilt = input$stateSelect)
  })
  
  output$standardGraphStateTitle <- renderText({
    paste("Here's what milk cap colors in ", 
          wordsList(input$stateSelect),
          " look like across the state:", 
          sep = "")
  })
  
  # Brand title line
  output$brandsStateTitle <- renderText({
    paste("These are the milk brands in ", 
          wordsList(input$stateSelect),
          " that we know of:", 
          sep = "")
  })
  
  # Chart of brands
  output$brandStateChart <- renderPlot({
    brands <- getBrands(stateFilt = input$stateSelect)
    if (length(brands$stylized) == 0) {
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste("Whoops!\n",
                                   "We're actually not too sure what brands of milk these responses represent.\n",
                                   "Help us out by adding your responses!"), 
           cex = 1.6, col = "black")
    } else {
      brandChart(brands$raw)
    }
  })
  
  
  
}