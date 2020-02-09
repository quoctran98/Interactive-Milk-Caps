library(shiny)
library(tidyverse)

allStates <- read.csv("otherData/abbreviations.csv")$State
allStates <- allStates[allStates != "Hawaii"]
allStates <- allStates[allStates != "Alaska"]
allStates <- allStates[allStates != "District of Columbia"]

fluidPage(
  
  titlePanel("What color are our milk caps?"),
  h4("by Quoc Tran (WIP)"),
  
  tabsetPanel(
    
    tabPanel(title = "Milk",
              sidebarLayout(
                 sidebarPanel(
                   selectInput("stateSelectLess", 
                               "Select One State", 
                               allStates,
                               selected = "California",
                               multiple = FALSE),
                   selectInput("stateSelect", 
                               "Add More States", 
                               allStates,
                               selected = "California",
                               multiple = TRUE),
                   hr(),
                   selectInput("milkfatSelect", 
                               "Milkfat", 
                               c("Whole", "Reduced", "Low", "Skim")),
                   plotOutput("mapCapsState")
                 ),
                 
                 mainPanel(
                   h4(textOutput("blurbState")),
                   h4("Help us out by adding more :)"),
                   hr(),
                   h3(textOutput("standardGraphStateTitle")),
                   fluidRow(
                     column(3, plotOutput("standardGraphStateWhole")),
                     column(3, plotOutput("standardGraphStateReduced")),
                     column(3, plotOutput("standardGraphStateLow")),
                     column(3, plotOutput("standardGraphStateSkim"))
                   ),
                   hr(),
                   h3(textOutput("brandsStateTitle")),
                   plotOutput("brandStateChart")
                 )
               )
    ),
  
    tabPanel(title = "Contribute",
             "I still haven't gotten to this yet, but you can fill out the Google Form. Thanks!",
             br(),
             url <- a("Google Form", href="https://forms.gle/rKGUBrTnuTDspPpu8")
    )
  )
)


