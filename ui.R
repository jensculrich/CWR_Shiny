library(tidyverse)
library(shiny)
library(shinythemes)

tib <- as_tibble(read.csv("Gardens.csv"))

ui <- fluidPage(theme = shinytheme("yeti"),
  navbarPage("Canadian Crop Wild Relative Inventory",
    tabPanel("BGCI GARDENS",
          sidebarPanel(
            selectInput("inProvince", "Select a Province", choices = tib$Province)
          ),
          mainPanel(
            tableOutput("gardenData")
          )
    )
  )
)
 