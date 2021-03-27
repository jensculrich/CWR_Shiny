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

# Load required data and shapefiles for building reactive maps and data tables
canada_ecoregions_geojson <- st_read("canada_ecoregions_clipped.geojson", quiet = TRUE)
canada_provinces_geojson <- st_read("canada_provinces.geojson", quiet = TRUE)
province_gap_table <- as_data_frame(read.csv("province_gap_table.csv"))
ecoregion_gap_table <- as_data_frame(read.csv("ecoregion_gap_table.csv"))
# order gap tables so that user choices are alphabetically organized
province_gap_table <- province_gap_table[order(province_gap_table$crop),]
ecoregion_gap_table <- ecoregion_gap_table[order(ecoregion_gap_table$crop),]


ui <- fluidPage(theme = shinytheme("yeti"),
  navbarPage("Inventory of Canadian Crop Wild Relatives (CWRs) in Botanic Gardens",
    
    tabPanel("About Crop Wild Relatives"
    ), # tabPanel "About Crop Wild Relatives"
    
    tabPanel("Explore CWR native ranges"
    ), # tabPanel "CWR native ranges"
    
    # update so that select input start is empty         
    tabPanel("Conduct a CWR Ex Situ Conservation Gap Analysis",
        sidebarPanel(
          # update underlying data frame with categories to facilitate interaction (e.g. fruit, vege, nut, tree)
          # add selectInput here
          # user chooses a crop of interest
          selectInput("inSelectedCrop", "Select a Crop", 
                      choices = province_gap_table$crop),
          # user chooses a CWR (filtered to match the selected crop)
          # update this so that user can choose a CWR without first selecting crop
          selectInput("inSelectedCWR", "Select a Crop Wild Relative", 
                      choices = province_gap_table$species),
          # user chooses to view map with ecoregion or province boundaries displayed
          selectInput("inProvincesOrEcoregions", "Choose a Geographic Display",
                      choices = c("Provinces", "Ecoregions")
          # could add a * noting that province is a subset of ecoregion (because ecoregion requires finer lat/long of origin)
          ) # selectInput 
        ), # sidebarPanel
        sidebarPanel(
          "note about province geo data versus ecoregion geo data (ecoregion required 
          recorded lat/long"
        ),
        mainPanel(
          # plot the geographic range and gaps
          plotOutput("gapPlot"),
          # provide summary data for the CWR
          tableOutput("gapTable")
          # could also add a picture of the CWR
        ) # add mainPanel
      
    ), # tabPanel("Conduct a CWR Ex Situ Conservation Gap Analysis")

    tabPanel("Acknowledgements",
             mainPanel("CWR collection data was contributed by: University of British
                       Columbia Botanic Garden (Vancouver, BC), Montreal Botanic Garden (Montreal, QC), 
                       University of Guelph Arboretum (Guelph, ON), Royal Botanic 
                       Garden (_, ON), etc...",
                       "Guidance and advice was provided by: Tara Moreau (UBC Botanic Garden),
                       Colin, Abby, Axel, Claire, Angela, etc.",
                       "Developed by ..."
             ) # mainPanel
    ) # tabPanel "Acknowledgements"
    
  ) # navbarPage
) # ui








