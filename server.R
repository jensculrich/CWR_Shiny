###############################
# server.R for CWR Shiny App  #
###############################

# Written by Jens Ulrich and Erika Luna Perez

########################################
# DATA WRANGLING AND SUPPORT FUNCTIONS #
########################################

# Load required data and shapefiles for building reactive maps and data tables
canada_ecoregions_geojson <- st_read("data/canada_ecoregions_clipped.geojson", quiet = TRUE)
canada_provinces_geojson <- st_read("data/canada_provinces.geojson", quiet = TRUE)
province_gap_table <- as_tibble(read.csv("data/province_gap_table_post_manual_range_edits.csv"))
ecoregion_gap_table <- as_tibble(read.csv("data/ecoregion_gap_table_post_manual_range_edits.csv"))

# Define map projection
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Define the mapping theme -- remove axes, ticks, borders, legends, etc.
# Come back to this and add a legend
theme_map <- function(base_size=9, base_family="") { # 3
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          # legend.justification = c(0,0), # no longer using a legend
          legend.position = "none"
    )
}

# spatially project the garden data
province_gap_table_sf <- st_as_sf(province_gap_table, 
                                  coords = c("longitude", "latitude"), 
                                  crs = 4326, 
                                  na.fail = FALSE) %>%
  filter(country == "Canada")

################
# SERVER LOGIC #
################

# Server Logic is separated by tabs 
# 1. Find native CWRs
# 2. Conduct a CWR Gap Analysis

shinyServer(function(input, output, session){

##################
#  NATIVE CWRs  #  
##################
  
# for native CWR tab,
# user can input province or ecoregion consideration
# and then generate a map and a table of native OR endemic CWRs by region focus
# to do so, the server side needs to generate plot and table data
# that's dependent on the users choices of geographic regions and variable of interest
# TO ADD: user choice to select Crop Category and Crop (but not individual CWRs)
# and then filter the datasets (use an observe function)
  
  # allow user to click on a polygon (region) and filter the CWR table to that region
  observe({ 
    
    ## give the user the ability to choose by hovering on the map
    event <- input$choroplethPlot_shape_click
    updateSelectInput(session, inputId = "inRegion", selected = event$id)
    
    z <- input$inNativeProvincesOrEcoregions
    if(z == "Ecoregions"){
      updateSelectInput(session, "inRegion", 
                        choices = ecoregion_gap_table$ECO_NAME,
                        selected = NULL)
      
    ## give the user the ability to choose by hovering on the map
    event <- input$choroplethPlot_shape_click
    updateSelectInput(session, inputId = "inRegion", selected = event$id)
    } 
  }) 
  
  # plot candian provinces or ecoregions colored by # of CWRs per region
  plotDataNativeRanges <- reactive({
    if(input$inNativeProvincesOrEcoregions == "Provinces"){ # if user chooses provinces...
      if(input$inTotalOrEndemic == "Identify All Native CWRs") { # if user chooses to show all CWRs...
        native_occurrence_heatmap_provinces <- province_gap_table %>%
          # group by province
          dplyr::group_by(province) %>%
          # distinct (since when there are >1 accessions for a species from the province the 
          # row gets expanded. We just want a count of one row per species found in the province)
          distinct(species, .keep_all = TRUE) %>%
          # tally the number of species
          add_tally() %>%
          rename("variable" = "n")
        } else{
          native_occurrence_heatmap_provinces <- province_gap_table %>%
          # distinct (since when there are >1 accessions for a species from the province the 
          # row gets expanded. We just want a count of one row per species found in the province)
          dplyr::group_by(province) %>%
          distinct(species, .keep_all = TRUE) %>%
          ungroup() %>%
          distinct(species, .keep_all = TRUE) %>%
          # identify endemic species per province
          # species that occur in only one province
          dplyr::group_by(species) %>%
          # if group is only one row, endemic = 1, else endemic = 0
          add_tally() %>%
          rename("native_provinces_for_species" = "n") %>%
          mutate(is_endemic = ifelse(
            native_provinces_for_species == 1, 1, 0)) %>%
          ungroup() %>%
          dplyr::group_by(province) %>%
          mutate(variable = sum(is_endemic))
          
        } # end nested else, endemics
      
      # join plot data with the spatial data frame necessary for projecting the plot  
      native_occurrence_sf_provinces <- tigris::geo_join(canada_provinces_geojson, native_occurrence_heatmap_provinces,  
                                                              by_df = "province", by_sp = "name")
      native_occurrence_sf_provinces <- native_occurrence_sf_provinces %>%
        rename("region" = "name")
      
    } else {
    # map by ecoregion
    if(input$inTotalOrEndemic == "Identify All Native CWRs") { # map natives
      native_occurrence_heatmap_ecoregion <- ecoregion_gap_table %>%
        # group by ecoregion
        dplyr::group_by(ECO_NAME) %>%
        # distinct (since when there are >1 accessions for a species from the province the 
        # row gets expanded. We just want a count of one row per species found in the province)
        distinct(species, .keep_all = TRUE) %>%
        # tally the number of species
        add_tally() %>%
        rename("variable" = "n") 
    } else{ # map endemics
      native_occurrence_heatmap_ecoregion <- ecoregion_gap_table %>%
        # distinct (since when there are >1 accessions for a species from the province the 
        # row gets expanded. We just want a count of one row per species found in the province)
        dplyr::group_by(ECO_NAME) %>%
        distinct(species, .keep_all = TRUE) %>%
        ungroup() %>%
        # identify endemic species per ecoregion
        # species that occur in only one ecoregion
        dplyr::group_by(species) %>%
        # if group is only one row, endemic = 1, else endemic = 0
        add_tally() %>%
        rename("native_ecoregions_for_species" = "n") %>%
        mutate(is_endemic = ifelse(
          native_ecoregions_for_species == 1, 1, 0)) %>%
        ungroup() %>%
        dplyr::group_by(ECO_NAME) %>%
        mutate(variable = sum(is_endemic))
      } # end nested else, endemics
      
      native_occurrence_sf_ecoregions <- tigris::geo_join(canada_ecoregions_geojson, native_occurrence_heatmap_ecoregion, 
                                                          by_sp = "ECO_NAME", by_df = "ECO_NAME")
      native_occurrence_sf_ecoregions <- native_occurrence_sf_ecoregions %>%
        rename("region" = "ECO_NAME")
      
    } # end else, ecoregions
    
  }) # end reactive plot data
  
  # want to filter this table when you click on a province 
  tableDataNativeRanges <- reactive({
    if(input$inNativeProvincesOrEcoregions == "Provinces"){
      if(input$inTotalOrEndemic == "Identify All Native CWRs") {
        native_occurrence_heatmap_provinces <- province_gap_table %>%
          # filter the table to the selected region
          filter(province == input$inRegion) %>%
          # group by province
          dplyr::group_by(province) %>%
          # distinct (since when there are >1 accessions for a species from the province the 
          # row gets expanded. We just want a count of one row per species found in the province)
          distinct(species, .keep_all = TRUE) %>%
          # tally the number of species
          add_tally() %>%
          dplyr::select(province, crop, species, Group, n) %>%
          rename("total CWRs in province" = "n") 
      } else{
        native_occurrence_heatmap_provinces <- province_gap_table %>%
          # identify endemic species per province
          # distinct (since when there are >1 accessions for a species from the province the 
          # row gets expanded. We just want a count of one row per species found in the province)
          dplyr::group_by(province) %>%
          distinct(species, .keep_all = TRUE) %>%
          ungroup() %>%
          # species that occur in only one province
          dplyr::group_by(species) %>%
          # if group is only one row, endemic = 1, else endemic = 0
          add_tally() %>%
          rename("native_provinces_for_species" = "n") %>%
          mutate(is_endemic = ifelse(
            native_provinces_for_species == 1, 1, 0)) %>%
          ungroup() %>%
          dplyr::group_by(province) %>%
          mutate(variable = sum(is_endemic)) %>%
          
          # filter the table to the selected region
          filter(province == input$inRegion) %>%
          filter(native_provinces_for_species == 1) %>%
          
          dplyr::select(province, Group, crop, species, variable) %>%
          rename("endemic CWRs in province" = "variable")
      } # end nested else, endemics
      
     } else {
      # map by ecoregion
      if(input$inTotalOrEndemic == "Identify All Native CWRs") { # map natives
        native_occurrence_heatmap_ecoregion <- ecoregion_gap_table %>%
          # filter the table to the selected region
          filter(ECO_NAME == input$inRegion) %>%
          # group by ecoregion
          dplyr::group_by(ECO_NAME) %>%
          # distinct (since when there are >1 accessions for a species from the province the 
          # row gets expanded. We just want a count of one row per species found in the province)
          distinct(species, .keep_all = TRUE) %>%
          # tally the number of species
          add_tally() %>%
          rename("variable" = "n") %>%
        
          dplyr::select(ECO_NAME, Group, crop, species, variable) %>%
          relocate(ECO_NAME, crop, species, Group, variable) %>%
          rename("total CWRs in ecoregion" = "variable")

      } else{ # map endemics
        native_occurrence_heatmap_ecoregion <- ecoregion_gap_table %>%
          # distinct (since when there are >1 accessions for a species from the province the 
          # row gets expanded. We just want a count of one row per species found in the province)
          dplyr::group_by(ECO_NAME) %>%
          distinct(species, .keep_all = TRUE) %>%
          ungroup() %>%
          # identify endemic species per ecoregion
          # species that occur in only one ecoregino
          dplyr::group_by(species) %>%
          # if group is only one row, endemic = 1, else endemic = 0
          add_tally() %>%
          rename("native_ecoregions_for_species" = "n") %>%
          mutate(is_endemic = ifelse(
            native_ecoregions_for_species == 1, 1, 0)) %>%
          ungroup() %>%
          dplyr::group_by(ECO_NAME) %>%
          mutate(variable = sum(is_endemic)) %>%
          
          # filter the table to the selected region
          filter(ECO_NAME == input$inRegion) %>%
          filter(native_ecoregions_for_species == 1) %>%
          
          dplyr::select(ECO_NAME, Group, crop, species, variable) %>%
          rename("endemic CWRs in ecoregion" = "variable")
      } # ended nested endmic else
    } # end else, ecoregions
  })
    
  # native range tab outputs: plot and table
  
  output$choroplethPlot <- renderLeaflet({
      
      # get data (will be ecoregoin or province, total native or just endemic CWR)
      mydat <- plotDataNativeRanges()    
      
      # Create a color palette for the map:
      mypalette <- colorNumeric( palette="YlOrBr", domain=mydat$variable, na.color="transparent")
      mypalette(c(45,43))
      
      # Prepare the text for tooltips:
      mytext <- paste(
        "Region: ", mydat$region,"<br/>", 
        "CWRs: ", mydat$variable, "<br/>", 
        sep="") %>%
        lapply(htmltools::HTML)
      
      # Basic choropleth with leaflet
      leaflet(plotDataNativeRanges()) %>% 
        addTiles()  %>% 
        setView( lat=60, lng=-98 , zoom=3) %>%
        addPolygons(fillOpacity = 0.5, 
                    smoothFactor = 0.5, 
                    color = ~colorNumeric("YlOrBr", variable)(variable),
                    label = mytext,
                    layerId = ~region) %>%
        addLegend( pal=mypalette, values=~variable, opacity=0.9, title = "CWRs", position = "bottomleft" )
    
  }) # end renderPlot
  
  output$nativeRangeTable <- DT::renderDataTable({
    datatable(tableDataNativeRanges(), 
              colnames = c("Region", "Crop", "Species", "Category", "CWRs in Region"))
  }) # end renderTable
  
##################
#  GAP ANALYSIS  #  
##################
  
  # filter the data set for a CWR of interest
  observe({ 
    # user chooses the group as the selected input
    x <- input$inSelectedGroup
    
    # filter the full gap table based on user selection
    filtered_CWRs <- filter(province_gap_table, province_gap_table$Group == x)
  
    # order filtered table so that user choices for CWR are alphabetically organized
    # to facilitate user choice
    filtered_CWRs <- filtered_CWRs[order(filtered_CWRs$crop),]
    
    updateSelectInput(session, "inSelectedCrop",
                      label = paste("Select a Crop"),
                      choices = filtered_CWRs$crop
    ) # updateSelectInput
    
    
    
  }) # observe
  
  observe({
    # After user selects a group, user may select a crop from within that group
    x <- input$inSelectedCrop

    filtered_CWRs <- filter(province_gap_table, province_gap_table$crop == x)

    filtered_CWRs <- filtered_CWRs[order(filtered_CWRs$species),]    
    # update select input so that CWRs choices are the subset related to the specified Crop
    updateSelectInput(session, "inSelectedCWR",
                      label = paste("Select a Crop Wild Relative"),
                      choices = filtered_CWRs$species
    ) # updateSelectInput
    
  })
  
  
  # plotData() is a reactive function that filters the gap table to provide 
  # necessary statistics for plotting (i.e. native range, coverage of native range, total accessions)
  # plotData() logically follows the user's choice of display:
  # IF user requests to display provinces, then generate province plot data
  # ELSE generate ecoregion plot data
  plotData <- reactive({ 
    if(input$inProvincesOrEcoregions == "Provinces"){
      # TRUE (user inputs "Provinces")
      # filter province_gap_table frame and calculate species specific stats
      provincePlotData <- province_gap_table %>%
        # filter to the user input CWR
        filter(province_gap_table$species == input$inSelectedCWR) %>%
        
        # tally the number of rows in each province with an existing accession (garden is not NA)
        dplyr::group_by(province) %>%
        add_tally(!is.na(garden)) %>%
        rename("accessions_in_province" = "n")  %>%
        ungroup() %>%
        
        # count the number of accessions w/ and w/out geographic data
        mutate(total_accessions_for_species = sum(!is.na(garden))) %>%
        mutate(accessions_no_geo_data = sum(is.na(province))) %>%
        mutate(accessions_with_geo_data = sum(!is.na(province))) %>%
        
        # convert number of accessions to a binary "is there or is there not an accession from x region"
        dplyr::group_by(province) %>%
        filter(row_number() == 1) %>%
        filter(!is.na(province)) %>%
        mutate(binary = ifelse(
          accessions_in_province > 0, 1, 0)) %>%
        ungroup() %>%
        
        # use the binary variable to determine the proportion of native regions with an accession
        mutate(num_native_province = sum(!duplicated(province))) %>%
        mutate(num_covered_province = sum(binary)) %>%
        mutate(perc_province_range_covered = 
                 num_covered_province / num_native_province) 
      
      # join plot data with the spatial data frame necessary for projecting the plot  
      tigris::geo_join(canada_provinces_geojson, provincePlotData,  
                         by_sp = "name", by_df = "province")
    
    } else{
      # FALSE, user inputs "Ecoregions"
      # filter province_gap_table frame and calculate species specific stats
      ecoregionPlotData <- ecoregion_gap_table %>%
        # filter to the user input CWR
        filter(ecoregion_gap_table$species == input$inSelectedCWR) %>%
        
        # tally the number of rows in each ecoregion with an existing accession (garden is not NA)
        dplyr::group_by(ECO_NAME) %>%
        add_tally(!is.na(garden)) %>%
        rename("accessions_in_ecoregion" = "n")  %>%
        ungroup() %>%
        
        # count the number of accessions w/ and w/out geographic data
        mutate(total_accessions_for_species = sum(!is.na(garden))) %>%
        mutate(accessions_no_geo_data = sum(is.na(ECO_NAME))) %>%
        mutate(accessions_with_geo_data = sum(!is.na(ECO_NAME))) %>%
        
        # convert number of accessions to a binary "is there or is there not an accession from x region"
        dplyr::group_by(ECO_NAME) %>%
        filter(row_number() == 1) %>%
        filter(!is.na(ECO_NAME)) %>%
        mutate(binary = ifelse(
          accessions_in_ecoregion > 0, 1, 0)) %>%
        ungroup() %>%
        
        # use the binary variable to determine the proportion of native regions with an accession
        mutate(num_native_ecoregions = sum(!duplicated(ECO_NAME))) %>%
        mutate(num_covered_ecoregions = sum(binary)) %>%
        mutate(perc_ecoregion_range_covered = 
                 num_covered_ecoregions / num_native_ecoregions) 
      
        # join plot data with the spatial data frame necessary for projecting the plot      
        tigris::geo_join(canada_ecoregions_geojson, ecoregionPlotData, by = "ECO_NAME")
    
    } # end else
    
  }) # end reactive
  
  # tableData() is a reactive function that filters the gap table to provide 
  # necessary statistics for a summary table (i.e. native range, coverage of native range, total accessions)
  # tableData() logically follows the user's choice of display:
  # IF user requests to display provinces, then generate province table data
  # ELSE generate ecoregion table data
  tableData <- reactive({ 
    
    if(input$inProvincesOrEcoregions == "Provinces"){
      # TRUE (user inputs "Provinces") 
      provinceTableData <- province_gap_table %>%
        # filter the table to the selected CWR
        filter(province_gap_table$species == input$inSelectedCWR) %>%
        
        # tally the number of rows in each province with an existing accession (garden is not NA)
        dplyr::group_by(province) %>%
        add_tally(!is.na(garden)) %>%
        rename("accessions_in_province" = "n")  %>%
        ungroup() %>%
        
        # count the number of accessions w/ and w/out geographic data
        mutate(accessions_no_geo_data = sum(is.na(province))) %>%
        mutate(accessions_with_geo_data = sum(!is.na(province) & !is.na(garden)))  %>%
        mutate(total_accessions_for_species = accessions_with_geo_data + accessions_no_geo_data) %>%
        
        # convert number of accessions to a binary "is there or is there not an accession from x region"
        dplyr::group_by(province) %>%
        filter(row_number() == 1) %>%
        filter(!is.na(province)) %>%
        mutate(binary = ifelse(
          accessions_in_province > 0, 1, 0)) %>%
        ungroup() %>%
        
        # use the binary variable to determine the proportion of native regions with an accession
        mutate(num_native_province = sum(!duplicated(province))) %>%
        mutate(num_covered_province = sum(binary)) %>%
        mutate(perc_province_range_covered = 
                 num_covered_province / num_native_province) %>%
        
        # format the data for the summary table 
        filter(row_number() == 1) %>% # for now only want one row (could adjust this with row per province)
        dplyr::select(num_native_province, num_covered_province,
                      accessions_with_geo_data, total_accessions_for_species) %>%
        mutate(num_covered_province = as.integer(num_covered_province)) %>%
        rename("native provinces" = num_native_province,
               "covered provinces" = num_covered_province,
               "accessions with geographic data" = accessions_with_geo_data,
               "total accessions" = total_accessions_for_species)
      
    } else {
      ecoregionTableData <- ecoregion_gap_table %>%
        # filter the table to the selected CWR
        filter(ecoregion_gap_table$species == input$inSelectedCWR) %>%
        
        # tally the number of rows in each ecoregion with an existing accession (garden is not NA)
        dplyr::group_by(ECO_NAME) %>%
        add_tally(!is.na(garden)) %>%
        rename("accessions_in_ecoregion" = "n")  %>%
        ungroup() %>%
        
        # count the number of accessions w/ and w/out geographic data
        mutate(accessions_no_geo_data = sum(is.na(ECO_NAME))) %>%
        mutate(accessions_with_geo_data = sum(!is.na(ECO_NAME) & !is.na(garden)))  %>%
        mutate(total_accessions_for_species = accessions_with_geo_data + accessions_no_geo_data) %>%
        
        # convert number of accessions to a binary "is there or is there not an accession from x region"
        dplyr::group_by(ECO_NAME) %>%
        filter(row_number() == 1) %>%
        filter(!is.na(ECO_NAME)) %>%
        mutate(binary = ifelse(
          accessions_in_ecoregion > 0, 1, 0)) %>%
        ungroup() %>%
        
        # use the binary variable to determine the proportion of native regions with an accession
        mutate(num_native_ecoregions = sum(!duplicated(ECO_NAME))) %>%
        mutate(num_covered_ecoregions = sum(binary)) %>%
        mutate(perc_ecoregion_range_covered = 
                 num_covered_ecoregions / num_native_ecoregions) %>%
        
        # format the data for the summary table 
        filter(row_number() == 1) %>%
        dplyr::select(num_native_ecoregions, num_covered_ecoregions,
                      accessions_with_geo_data, total_accessions_for_species) %>%
        mutate(num_covered_ecoregions = as.integer(num_covered_ecoregions)) %>%
        rename("native ecoregions" = num_native_ecoregions,
               "covered ecoregions" = num_covered_ecoregions,
               "accessions with geographic data" = accessions_with_geo_data,
               "total accessions" = total_accessions_for_species)
    }
    
      
  })
    
    # add plot to the main panel using the reactive plotData() function
    output$gapPlot <- renderPlot({
      
      # validate allows us to share a prompt (rather than an error message until a CWR is chosen)
      shiny::validate(
        need(input$inSelectedCrop, "")
      )
      
      subset_gap_table_sf <- province_gap_table_sf %>%
        filter(species == input$inSelectedCWR)
      
      # use ggplot to map the native range and conserved accessions  
      ggplot(plotData()) +
      geom_sf(aes(fill = as.factor(binary)),
        color = "gray60", size = 0.1) +
        geom_sf(data = subset_gap_table_sf, color = 'skyblue', alpha = 0.8, size = 4) + 
        coord_sf(crs = crs_string) +
      scale_fill_manual(values = c("0" = "gray80", "1" = "gray18"), 
                        labels = c("No accessions with geographic data held in collection", 
                                   "1 or more accession with geographic data held in collection", 
                                   "Outside of native range")) +
      theme_map() +
      ggtitle("") +
      theme(panel.grid.major = element_line(color = "white"),
            plot.title = element_text(color="black",
            size=10, face="bold.italic", hjust = 0.5),
            legend.text = element_text(size=10))

    }) # end renderPlot renderDataTable
    
    # add gap table to the main panel using the reactive tableData() function
    output$gapTable <- DT::renderDataTable({
      datatable(tableData(), 
                colnames = c("Native Regions", "Regions Represented by Garden Collections", "Garden Accessions w/ Geographic Data", "Total Garden Accesions"))
    }) # end renderTable
    
}) # server