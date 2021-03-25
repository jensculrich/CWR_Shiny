# add another table panel with interactive native range heat map 
# and endemic heat map (to promote further in situ conservation)

# add an about tab, with brief description
# and summary statistics?
# and garden contributors/acknowledgments?

library(shiny)
library(shinythemes)

library(sf) # the base package manipulating shapes
library(rgeos)
library(rgdal) # geo data abstraction library
library(geojsonio) # geo json input and output
library(spdplyr) # the `dplyr` counterpart for shapes
library(rmapshaper) # the package that allows geo shape transformation
library(magrittr) # data wrangling
library(dplyr) # data wrangling
library(tidyverse) # data wrangling
library(ggplot2) # plotting
library(raster) 
library(viridis) # for colour schemes
library(tigris) # for joining spatial data with data frame classes

canada_ecoregions_geojson <- st_read("canada_ecoregions_clipped.geojson", quiet = TRUE)
canada_provinces_geojson <- st_read("canada_provinces.geojson", quiet = TRUE)
full_gap_table <- as_data_frame(read.csv("full_gap_table.csv"))

ui <- fluidPage(theme = shinytheme("yeti"),
  navbarPage("Canadian Crop Wild Relative Inventory",
    
    tabPanel("About Crop Wild Relatives"
    ), # tabPanel "About Crop Wild Relatives"
    
    tabPanel("CWR native ranges"
    ), # tabPanel "CWR native ranges"
    
    # update so that select input start is empty         
    tabPanel("Conduct a CWR Ex Situ Conservation Gap Analysis",
        sidebarPanel(
          # need to sort these alphabetically
          selectInput("inSelectedCrop", "Select a Crop", 
                      choices = full_gap_table$crop),
          # need to filter the list to all CWRs related to the Crop
          selectInput("inSelectedCWR", "Select a Crop Wild Relative", 
                      choices = full_gap_table$species),
          selectInput("inProvincesOrEcoregions", "Choose a Geographic Display",
                      choices = c("Provinces", "Ecoregions")
          )
        ), # sidebarPanel
        mainPanel(
          plotOutput("gapPlot"),
          tableOutput("gapTable")
        ) # add mainPanel
      
    ), # tabPanel("Conduct a CWR Ex Situ Conservation Gap Analysis")

    tabPanel("Acknowledgements"
    ) # tabPanel "Acknowledgements"
    
  ) # navbarPage
) # ui








