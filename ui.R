library(tidyverse)
library(shiny)
library(shinythemes)

tib <- as_tibble(read.csv("Gardens.csv"))

ui <- fluidPage(theme = shinytheme("cerulean"),
  # *Input functions
#  sliderInput(inputId = "num",
#              label = "choose a number",
#              value = 25, min = 1, max = 100),
  # *Output functions
#  plotOutput("hist"),

  # Input functions 
  selectizeInput('var1', 'Select Province', 
                 choices = c("choose" = "", levels(tib$Province))),
  # Output functions
  tableOutput("table")
)




#shinyApp(
#  ui <- fluidPage(theme = shinytheme("yeti"), # can search for shiny apps online
#                  navbarPage(
#                    "Canadian Crop Wild Relative Inventory", # name of the app
#                    tabPanel("Navbar 1",
#                             sidebarPanel(selectizeInput('var1', 'Select Province', choices = c("choose" = "", levels(tib$Province)))
#                             ), # sidebarPanel
#                             mainPanel(
#                               tableOutput("table")
#                             ) # mainPanel
#                    )
#                  )
#  ),
  
