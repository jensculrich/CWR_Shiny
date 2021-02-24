library(tidyverse)
library(shiny)
library(shinythemes)

tib <- as_tibble(read.csv("Gardens.csv"))

ui <- fluidPage(theme = shinytheme("yeti"),
        navbarPage("Canadian Crop Wild Relative Inventory", # name of the app
          tabPanel("README" 
            # First tab could display information about the project and app purposes
          ),
          tabPanel("BGCI Gardens", # Panel 2, search through gardens
            sidebarPanel(
              # Input functions 
              selectizeInput('var1', 'Select Province', # Filter all gardens by province
                             choices = c("choose" = "", levels(tib$Province))),
            ), # sidebarPanel
            mainPanel(  
              # Output functions
              tableOutput("table") # Table of Gardens
            ) # mainPanel 
          ) # tabPanel "BGCI Gardens"
        ) # navbarPage
) # fluidPage

# test commit
