# in progress: updateSelectInput so that user Crop selection filters 
# down second selection box to only related CWRs
# also want these drop down menus in alphabetical order

library(shiny)
library(shinythemes)

library(sf) # the base package manipulating shapes
library(rgeos)
library(rgdal) # geo data abstraction library
library(geojsonio) # geo json input and output
library(spdplyr) # the `dplyr` counterpart for shapes
library(rmapshaper) # the package that allows geo shape transformation
library(magrittr) # data wrangling
library(dplyr)
library(tidyverse)
library(ggplot2)
library(raster)
library(cartography)
library(viridis)
library(tigris)

# do we need to add objects to the environmnet in server and ui or are they global?
gardens <- as_data_frame(read.csv("Gardens.csv"))
canada_ecoregions_geojson <- st_read("canada_ecoregions_clipped.geojson", quiet = TRUE)
canada_provinces_geojson <- st_read("canada_provinces.geojson", quiet = TRUE)
full_gap_table <- as_data_frame(read.csv("full_gap_table.csv"))


ui <- fluidPage(theme = shinytheme("yeti"),
  navbarPage("Canadian Crop Wild Relative Inventory",
    # The BGCI GARDENS IS MEANT TO ACT AS A DUMMY PANEL TO TEST THAT THE APP LAUNCHES 
    tabPanel("BGCI GARDENS",
        sidebarPanel(
          selectInput("inProvince", "Select a Province", 
                      choices = gardens$Province)
        ),
        mainPanel(
          tableOutput("gardenData")
        )
    ), # tabPanel("BGCI GARDENS)
    
    # add another tabPanel here
    tabPanel("Conduct a CWR Ex Situ Conservation Gap Analysis",
        sidebarPanel(
          # need to sort these alphabetically
          selectInput("inSelectedCrop", "Select a Crop", 
                      choices = full_gap_table$crop),
          # need to filter the list to all CWRs related to the Crop
          selectInput("inSelectedCWR", "Select a Crop Wild Relative", 
                      choices = full_gap_table$species)
        ), # sidebarPanel
        mainPanel(
          plotOutput("gapPlot"),
          tableOutput("gapTable")
        ) # add mainPanel
      
    ) # tabPanel("Conduct a CWR Ex Situ Conservation Gap Analysis")
  ) # navbarPage
) # ui
 