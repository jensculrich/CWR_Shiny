###########################
# ui.R for CWR Shiny App  #
###########################

# Written by Jens Ulrich and Erika Luna Perez

###########################
# LIBRARIES               #
###########################

library(shiny)
library(shinythemes)
library(sf) # the base package manipulating shapes
library(spdplyr) # the `dplyr` counterpart for shapes
library(dplyr) # data wrangling
library(tidyverse) # data wrangling
library(ggplot2) # plotting
library(tigris) # for joining spatial data with data frame classes
library(leaflet)
library(htmltools)
library(shinydashboard)
library(DT)
library(rgeos)
library(rgdal)
library(rmapshaper)
library(sp)
library(grid)

########################################
# DATA WRANGLING AND SUPPORT FUNCTIONS #
########################################

# Load required data and shapefiles for  
# building reactive maps and data tables 

# canada_ecoregions_geojson defines ecoregions in Canada, clipped to the national border of Canada
canada_ecoregions_geojson <- st_read("data/canada_ecoregions_clipped.geojson", quiet = TRUE)
# canada_provinces_geojson defines province and territory boundaries
canada_provinces_geojson <- st_read("data/canada_provinces.geojson", quiet = TRUE)

# province_gap_table includes all garden accessions from our surveyed gardens
# with lat/long when applicable (needs to be formatted here or before uploading)
# The table has a row for each native province that a species is native to with garden = NA
# along with a row for each garden accession from each native province.
# ecoregion_gap_table has similar setup
province_gap_table <- as_tibble(read.csv("data/province_gap_table_post_manual_range_edits.csv"))
ecoregion_gap_table <- as_tibble(read.csv("data/ecoregion_gap_table_post_manual_range_edits.csv"))

# order gap tables so that user choices are alphabetically organized
province_gap_table <- province_gap_table[order(province_gap_table$crop),]
ecoregion_gap_table <- ecoregion_gap_table[order(ecoregion_gap_table$crop),]


dbHeader <- dashboardHeader(title = "My Dashboard",
                            tags$li(a(href = 'http://shinyapps.company.com',
                                      icon("power-off"),
                                      title = "Back to Apps Home"),
                                    class = "dropdown"),
                            tags$li(a(href = 'http://www.company.com',
                                      img(src = 'ubc.png',
                                          title = "Company Home", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"))

###########
# LOAD UI #
###########

# ui structure: one navbar page with 4 tab panels

ui <- fluidPage(
  
  #theme = shinytheme("yeti"),
  # load custom stylesheet
  includeCSS("www/style.css"),
  
  dashboardPage(
    
    skin = "purple",
    
    dashboardHeader(title = "Conservation of CWR in Canada", titleWidth = 500),
    
    dashboardSidebar(
      
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("What are CWRs?", tabName = "about", icon = icon("seedling")),
        menuItem("Find Native CWRs", tabName = "find", icon = icon("thumbtack")),
        menuItem("Conservation in Gardens", tabName = "explore", icon = icon("map marked alt")),
        menuItem("Acknowledgements", tabName = "aknow", icon = icon("tasks"))
        
      )
      
    ),
    
    dashboardBody(
      
      tabItems(
        # First tab content
        tabItem(tabName = "home",
               
                includeMarkdown("www/home.md")
        ),
        
        # Second tab content
        tabItem(tabName = "about",
                             includeMarkdown("www/about.md")
                            
                           
                  ),
        
        # Third tab content
        tabItem(tabName = "find",
                
                includeMarkdown("www/find.md"),
                
                fluidRow(
                  box(#title = "Find Native CWRs",
                      #solidHeader = T,
                      width = 4,
                      collapsible = T,
                      
                      selectInput("inTotalOrEndemic", "What would you like to do?",
                                  choices = c("Identify All Native CWRs", "Identify Endemic CWRs")),
                      # user chooses to view map with ecoregion or province boundaries displayed
                      selectInput("inNativeProvincesOrEcoregions", "Choose a Geographic Display*",
                                  choices = c("Provinces", "Ecoregions")),
                      # want to update this so it's dependnet on users choice of provinces v. ecoregions
                      selectInput("inRegion", "Filter CWR List by a Region:", 
                                  choices = c("Alberta", "British Columbia", "Manitoba", "Newfoundland and Labrador",
                                              "New Brunswick", "Northwest Territories", "Nova Scotia", "Nunavut", 
                                              "Ontario", "Prince Edward Island", "Quebec", 
                                              "Saskatchewan", "Yukon"))
                      ),
                  
                  box(#title = "Range map", solidHeader = T,
                      width = 8, collapsible = T,
                      leafletOutput("choroplethPlot"),
                      dataTableOutput("nativeRangeTable"))
                ) # row
                
                ), # end tabPanel "CWR native ranges"
        
        # update so that select input start is empty         
        tabItem(tabName = "explore",
                
                includeMarkdown("www/explore.md"),
                
                fluidRow(
                  box(#title = "Find Native CWRs",
                    #solidHeader = T,
                    width = 4,
                    collapsible = T,
                    
                    # user chooses a group of interest
                    selectInput("inSelectedGroup", "Select a Crop Category", 
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
                                choices = c("Provinces", "Ecoregions"))
                                # could add a * noting that province is a subset of ecoregion (because ecoregion requires finer lat/long of origin)
                  ),
                  
                  box(#title = "Range map", solidHeader = T,
                    width = 8, collapsible = T,
                    plotOutput("gapPlot"),
                    includeMarkdown("www/conservation_tab.md"),
                    # provide summary data for the CWR
                    dataTableOutput("gapTable")
                ) # box
                
                ) #row
                

                 ), # end tabPanel("Conduct a CWR Ex Situ Conservation Gap Analysis")
        
        tabItem(tabName = "aknow", 
                
                includeMarkdown("www/aknow.md")
        )
          ) # tabPanel "Acknowledgements"
        
        )
      )
) # ui

