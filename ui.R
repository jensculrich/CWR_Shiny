###########################
# ui.R for CWR Shiny App  #
###########################

###########################
# LIBRARIES               #
###########################

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
library(leaflet)
library(htmltools)

########################################
# DATA WRANGLING AND SUPPORT FUNCTIONS #
########################################

# Load required data and shapefiles for  
# building reactive maps and data tables 

# canada_ecoregions_geojson defines ecoregions in Canada, clipped to the national border of Canada
canada_ecoregions_geojson <- st_read("canada_ecoregions_clipped.geojson", quiet = TRUE)
# canada_provinces_geojson defines province and territory boundaries
canada_provinces_geojson <- st_read("canada_provinces.geojson", quiet = TRUE)

# province_gap_table includes all garden accessions from our surveyed gardens
# with lat/long when applicable (needs to be formatted here or before uploading)
# The table has a row for each native province that a species is native to with garden = NA
# along with a row for each garden accession from each native province.
# ecoregion_gap_table has similar setup
province_gap_table <- as_tibble(read.csv("province_gap_table.csv"))
ecoregion_gap_table <- as_tibble(read.csv("ecoregion_gap_table.csv"))

# order gap tables so that user choices are alphabetically organized
province_gap_table <- province_gap_table[order(province_gap_table$crop),]
ecoregion_gap_table <- ecoregion_gap_table[order(ecoregion_gap_table$crop),]

###########
# LOAD UI #
###########

# ui structure: one navbar page with 4 tab panels

ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage("Inventory of Canadian Crop Wild Relatives (CWRs) in Botanic Gardens",
                           
                           # an introduction to crop wild relatives and the unique
                           # potential of botanic gardens for promoting their conservation
                           tabPanel("About Crop Wild Relatives",
                                    mainPanel(
                                      includeMarkdown("www/about.md"),
                                      img(src = "saskatoon_berry.png", height = 200, width = 300)
                                    )     
                                    
                           ), # end tabPanel "About Crop Wild Relatives"
                           
                           tabPanel("Find native CWRs",
                                    sidebarPanel(
                                      # input, what would you like to do?
                                      selectInput("inTotalOrEndemic", "What would you like to do?",
                                                  choices = c("Identify All Native CWRs", "Identify Endemic CWRs")),
                                      # user chooses to view map with ecoregion or province boundaries displayed
                                      selectInput("inNativeProvincesOrEcoregions", "Choose a Geographic Display*",
                                                  choices = c("Provinces", "Ecoregions")),
                                      # want to update this so it's dependnet on users choice of provinces v. ecoregions
                                      selectInput("inRegion", "Filter CWR List by a Region:", 
                                                  choices = province_gap_table$province)
                                    ), # end sidebarPanel
                                    mainPanel(
                                      leafletOutput("choroplethPlot"),
                                      tableOutput("nativeRangeTable")
                                    ) # end mainPanel
                           ), # end tabPanel "CWR native ranges"
                           
                           # update so that select input start is empty         
                           tabPanel("Explore CWR Conservation in Botanic Gardens",
                                    sidebarPanel(
                                      
                                      # update underlying data frame with categories to facilitate interaction (e.g. fruit, vege, nut, tree)
                                      # add selectInput here
                                      # user chooses a group of interest
                                      selectInput("inSelectedGroup", "Select a group", 
                                                  choices = province_gap_table$Group),
                                      # user chooses a crop of interest
                                      selectInput("inSelectedCrop", "Select a Crop", 
                                                  choices = province_gap_table$crop),
                                      # user chooses a CWR (filtered to match the selected crop)
                                      # update this so that user can choose a CWR without first selecting crop
                                      selectInput("inSelectedCWR", "Select a Crop Wild Relative", 
                                                  choices = province_gap_table$species),
                                      # user chooses to view map with ecoregion or province boundaries displayed
                                      selectInput("inProvincesOrEcoregions", "Choose a Geographic Display*",
                                                  choices = c("Provinces", "Ecoregions"),
                                                  # could add a * noting that province is a subset of ecoregion (because ecoregion requires finer lat/long of origin)
                                      ), # end selectInput 
                                      "*note: a larger proportion of accessions are associated with coarser scale 
          province versus finer scale ecoregion geographic origin information"
                                    ), # end sidebarPanel
                                    
                                    mainPanel(
                                      # plot the geographic range and gaps
                                      plotOutput("gapPlot"),
                                      # provide summary data for the CWR
                                      tableOutput("gapTable")
                                      # could also add a picture of the CWR
                                    ) # end mainPanel
                                    
                           ), # end tabPanel("Conduct a CWR Ex Situ Conservation Gap Analysis")
                           
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

