library(tidyverse)
library(shiny)
library(shinythemes)

tib <- as_tibble(read.csv("Gardens.csv"))

ui <- fluidPage(theme = shinytheme("yeti"),
        navbarPage("Canadian Crop Wild Relative Inventory", # name of the app
          tabPanel("README", 
            h1("add title text here"),
            h4("add descriptive text here")
            # First tab could display information about the project and app purposes
          ),
          tabPanel("BGCI Gardens", # Panel 2, search through gardens
            sidebarPanel(
              # Input functions 
              h3("BGCI affiliate gardens by province"),
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

# try adding a selectivize input on a new tab to search through CWR Master list
# does it require to server functions to do this?
