library(tidyverse)
library(shiny)
library(shinythemes)

tibGardens <- as_tibble(read.csv("Gardens.csv"))

ui <- fluidPage(theme = shinytheme("yeti"),
  navbarPage("Canadian Crop Wild Relative Inventory",
    # The BGCI GARDENS IS MEANT TO ACT AS A DUMMY PANEL TO TEST THAT THE APP LAUNCHES 
    tabPanel("BGCI GARDENS",
        sidebarPanel(
          selectInput("inProvince", "Select a Province", choices = tibGardens$Province)
        ),
        mainPanel(
          tableOutput("gardenData")
        )
    ) # tabPanel("BGCI GARDENS)
    # add another tabPanel here
    
  ) #navbarPage
) # UI
 