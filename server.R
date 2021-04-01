# big things to update: 
# 1 a native distribution tab (possibly display heatmaps for all crop wild relatives or major groups?)
# 3 add figure legend, notes for interpretation
# 4 add crop categories and re-upload and add as a selctInput (make it so that you don't necessarily HAVE to choose category)
# 5 add individual geo points to the map (does it make sense if some are only for province?)
# 6 hover over province shows number of accessions from province.
# 7 add acknowledgements (list of gardens that contributed data, Tara, Colin, Abby, Axel)
# 8 add the about tab: what are CWRs, why conserve across range, role of botanic gardens.
# 9 add associated wiki picture of genus

# Load required data and shapefiles for building reactive maps and data tables
canada_ecoregions_geojson <- st_read("canada_ecoregions_clipped.geojson", quiet = TRUE)
canada_provinces_geojson <- st_read("canada_provinces.geojson", quiet = TRUE)
province_gap_table <- as_data_frame(read.csv("province_gap_table.csv"))
ecoregion_gap_table <- as_data_frame(read.csv("ecoregion_gap_table.csv"))


# Define map projection
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Define the mappping theme -- remove axes, ticks, borders, legends, etc.
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
          legend.justification = c(0,0),
          legend.position = c(-.25,0)
    )
}

# edit geometry on gap tables so that the points can be spatially projected


shinyServer(function(input, output, session){

##################
#  NATIVE RANGE  #  
##################
  
# for native range tab,
# user can input province or ecoregion consideration
# and then generate a map and a table of native OR endemic CWRs by region focus
# to do so, the server side needs to generate plot and table data
# that's dependent on the users choices of geographic regions and variable of interest
# TO ADD: user choice to select Crop Category and Crop (but not individual CWRs)
# and then filter the datasets (use an observe function)
  
  plotDataNativeRanges <- reactive({
    if(input$inNativeProvincesOrEcoregions == "Provinces"){
      if(input$inTotalOrEndemic == "Map Native CWRs") {
        native_occurrence_heatmap_provinces <- province_gap_table %>%
          # filter for garden = NA
          filter(is.na(garden)) %>%
          # group by province
          group_by(province) %>%
          # tally the number of species
          add_tally() %>%
          rename("variable" = "n")
        } else{
          native_occurrence_heatmap_provinces <- province_gap_table %>%
          # filter for garden = NA
          filter(is.na(garden)) %>%
          # identify endemic species per province
          # species that occur in only one province
          group_by(species) %>%
          # if group is only one row, endemic = 1, else endemic = 0
          add_tally() %>%
          rename("native_provinces_for_species" = "n") %>%
          mutate(is_endemic = ifelse(
            native_provinces_for_species == 1, 1, 0)) %>%
          ungroup() %>%
          group_by(province) %>%
          mutate(variable = sum(is_endemic))
        } # end nested else, endemics
      
      # join plot data with the spatial data frame necessary for projecting the plot  
      native_occurrence_heatmap_provinces <- tigris::geo_join(canada_provinces_geojson, native_occurrence_heatmap_provinces,  
                                                              by_sp = "name", by_df = "province")
    } else {
    # map by ecoregion
    if(input$inTotalOrEndemic == "Map Native CWRs") { # map natives
      native_occurrence_heatmap_ecoregion <- ecoregion_gap_table %>%
        # filter for garden = NA
        filter(is.na(garden)) %>%
        # group by ecoregion
        group_by(ECO_NAME) %>%
        # tally the number of species
        add_tally() %>%
        rename("variable" = "n")
    } else{ # map endemics
      native_occurrence_heatmap_ecoregion <- ecoregion_gap_table %>%
        # identify endemic species per ecoregion
        # species that occur in only one ecoregion
        group_by(species) %>%
        # if group is only one row, endemic = 1, else endemic = 0
        add_tally() %>%
        rename("native_ecoregions_for_species" = "n") %>%
        mutate(is_endemic = ifelse(
          native_ecoregions_for_species == 1, 1, 0)) %>%
        ungroup() %>%
        group_by(ECO_NAME) %>%
        mutate(variable = sum(is_endemic))
      } # end nested else, endemics
      
      native_occurrence_sf_ecoregions <- tigris::geo_join(canada_ecoregions_geojson, native_occurrence_heatmap_ecoregion, by = "ECO_NAME")
    } # end else, ecoregions
    
  }) # end reactive plot data
  
  # want to filter this table when you click on a province 
  tableDataNativeRanges <- reactive({
    if(input$inNativeProvincesOrEcoregions == "Provinces"){
      if(input$inTotalOrEndemic == "Map Native CWRs") {
        native_occurrence_heatmap_provinces <- province_gap_table %>%
          # filter for garden = NA
          filter(is.na(garden)) %>%
          # group by province
          group_by(province) %>%
          # tally the number of species
          add_tally() %>%
          rename("variable" = "n")
      } else{
        native_occurrence_heatmap_provinces <- province_gap_table %>%
          # filter for garden = NA
          filter(is.na(garden)) %>%
          # identify endemic species per province
          # species that occur in only one province
          group_by(species) %>%
          # if group is only one row, endemic = 1, else endemic = 0
          add_tally() %>%
          rename("native_provinces_for_species" = "n") %>%
          mutate(is_endemic = ifelse(
            native_provinces_for_species == 1, 1, 0)) %>%
          ungroup() %>%
          group_by(province) %>%
          mutate(variable = sum(is_endemic))
      } # end nested else, endemics
     } else {
      # map by ecoregion
      if(input$inTotalOrEndemic == "Map Native CWRs") { # map natives
        native_occurrence_heatmap_ecoregion <- ecoregion_gap_table %>%
          # filter for garden = NA
          filter(is.na(garden)) %>%
          # group by ecoregion
          group_by(ECO_NAME) %>%
          # tally the number of species
          add_tally() %>%
          rename("variable" = "n")
      } else{ # map endemics
        native_occurrence_heatmap_ecoregion <- ecoregion_gap_table %>%
          # identify endemic species per ecoregion
          # species that occur in only one ecoregion
          group_by(species) %>%
          # if group is only one row, endemic = 1, else endemic = 0
          add_tally() %>%
          rename("native_ecoregions_for_species" = "n") %>%
          mutate(is_endemic = ifelse(
            native_ecoregions_for_species == 1, 1, 0)) %>%
          ungroup() %>%
          group_by(ECO_NAME) %>%
          mutate(variable = sum(is_endemic))
      } # end nested else, endemics
    } # end else, ecoregions
  })
    
  # native range tab outputs: plot and table
  
  output$choroplethPlot <- renderPlot({
    
      # use ggplot to map the native range and conserved accessions  
      ggplot(plotDataNativeRanges()) +
        geom_sf(aes(fill = variable),
                color = "gray60", size = 0.1) +
        coord_sf(crs = crs_string) +
        scale_fill_distiller(palette = "Spectral") +
        theme_map() +
        ggtitle("") +
        theme(panel.grid.major = element_line(color = "white"),
              plot.title = element_text(color="black",
                                        size=10, face="bold.italic", hjust = 0.5),
              legend.text = element_text(size=10))
    
  }) # end renderPlot
  
  output$nativeRangeTable <- renderTable({
    tableDataNativeRanges()
  }) # end renderTable

  
##################
#  GAP ANALYSIS  #  
##################
  
  # filter the data set for a CWR of interest
  observe({ 
    # user chooses the crop as the selected input
    x <- input$inSelectedCrop
    
    # filter the full gap table based on user selection
    filtered_CWRs <- filter(province_gap_table, province_gap_table$crop == x)
    
    # order filtered table so that user choices for CWR are alphabetically organized
    # to facilitate user choice
    filtered_CWRs <- filtered_CWRs[order(filtered_CWRs$species),]
    
    # update select input so that CWRs choices are the subset related to the specified Crop
    updateSelectInput(session, "inSelectedCWR",
                      label = paste("Select a Crop Wild Relative"),
                      choices = filtered_CWRs$species
    ) # updateSelectInput
    
  }) # observe
  
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
        group_by(province) %>%
        add_tally(!is.na(garden)) %>%
        rename("accessions_in_province" = "n")  %>%
        ungroup() %>%
        
        # count the number of accessions w/ and w/out geographic data
        mutate(total_accessions_for_species = sum(!is.na(garden))) %>%
        mutate(accessions_no_geo_data = sum(is.na(province))) %>%
        mutate(accessions_with_geo_data = sum(!is.na(province))) %>%
        
        # convert number of accessions to a binary "is there or is there not an accession from x region"
        group_by(province) %>%
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
        group_by(ECO_NAME) %>%
        add_tally(!is.na(garden)) %>%
        rename("accessions_in_ecoregion" = "n")  %>%
        ungroup() %>%
        
        # count the number of accessions w/ and w/out geographic data
        mutate(total_accessions_for_species = sum(!is.na(garden))) %>%
        mutate(accessions_no_geo_data = sum(is.na(ECO_NAME))) %>%
        mutate(accessions_with_geo_data = sum(!is.na(ECO_NAME))) %>%
        
        # convert number of accessions to a binary "is there or is there not an accession from x region"
        group_by(ECO_NAME) %>%
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
        group_by(province) %>%
        add_tally(!is.na(garden)) %>%
        rename("accessions_in_province" = "n")  %>%
        ungroup() %>%
        
        # count the number of accessions w/ and w/out geographic data
        mutate(accessions_no_geo_data = sum(is.na(province))) %>%
        mutate(accessions_with_geo_data = sum(!is.na(province) & !is.na(garden)))  %>%
        mutate(total_accessions_for_species = accessions_with_geo_data + accessions_no_geo_data) %>%
        
        # convert number of accessions to a binary "is there or is there not an accession from x region"
        group_by(province) %>%
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
                      accessions_with_geo_data, accessions_no_geo_data, total_accessions_for_species) %>%
        mutate(num_covered_province = as.integer(num_covered_province)) %>%
        rename("native provinces" = num_native_province,
               "covered provinces" = num_covered_province,
               "accessions with geographic data" = accessions_with_geo_data,
               "accessions lacking geographic data" = accessions_no_geo_data,
               "total accessions" = total_accessions_for_species)
      
    } else {
      ecoregionTableData <- ecoregion_gap_table %>%
        # filter the table to the selected CWR
        filter(ecoregion_gap_table$species == input$inSelectedCWR) %>%
        
        # tally the number of rows in each ecoregion with an existing accession (garden is not NA)
        group_by(ECO_NAME) %>%
        add_tally(!is.na(garden)) %>%
        rename("accessions_in_ecoregion" = "n")  %>%
        ungroup() %>%
        
        # count the number of accessions w/ and w/out geographic data
        mutate(accessions_no_geo_data = sum(is.na(ECO_NAME))) %>%
        mutate(accessions_with_geo_data = sum(!is.na(ECO_NAME) & !is.na(garden)))  %>%
        mutate(total_accessions_for_species = accessions_with_geo_data + accessions_no_geo_data) %>%
        
        # convert number of accessions to a binary "is there or is there not an accession from x region"
        group_by(ECO_NAME) %>%
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
                      accessions_with_geo_data, accessions_no_geo_data, total_accessions_for_species) %>%
        mutate(num_covered_ecoregions = as.integer(num_covered_ecoregions)) %>%
        rename("native ecoregions" = num_native_ecoregions,
               "covered ecoregions" = num_covered_ecoregions,
               "accessions with geographic data" = accessions_with_geo_data,
               "accessions lacking geographic data" = accessions_no_geo_data,
               "total accessions" = total_accessions_for_species)
    }
    
      
  })
    
    # add plot to the main panel using the reactive plotData() function
    output$gapPlot <- renderPlot({
      
      # validate allows us to share a prompt (rather than an error message until a CWR is chosen)
      shiny::validate(
        need(input$inSelectedCrop, 'Select a crop to filter the list of crop wild relatives')
      )
      
      # use ggplot to map the native range and conserved accessions  
      ggplot(plotData()) +
      geom_sf(aes(fill = as.factor(binary)),
        color = "gray60", size = 0.1) +
      coord_sf(crs = crs_string) +
      scale_fill_manual(values = c("gray80", "gray18"), 
                        labels = c("No accessions with geographic data held in collection", 
                                   ">1 accession with geographic data held in collection", 
                                   "Outside of native range")) +
      guides(fill = guide_legend(title = "Conservation Status in Botanic Gardens", 
                    title.position = "top",
                    title.theme = element_text(size = 10, face = "bold")
                    )) +
      theme_map() +
      ggtitle("") +
      theme(panel.grid.major = element_line(color = "white"),
            plot.title = element_text(color="black",
            size=10, face="bold.italic", hjust = 0.5),
            legend.text = element_text(size=10))

    }) # end renderPlot
    
    # add gap table to the main panel using the reactive tableData() function
    output$gapTable <- renderTable({
      tableData()
    }) # renderTable
    
}) # server