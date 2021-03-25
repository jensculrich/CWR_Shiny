# Load required data and shapefiles for building reactive maps and data tables
canada_ecoregions_geojson <- st_read("canada_ecoregions_clipped.geojson", quiet = TRUE)
canada_provinces_geojson <- st_read("canada_provinces.geojson", quiet = TRUE)
full_gap_table <- as_data_frame(read.csv("full_gap_table.csv"))

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
          legend.position = c(0,0)
    )
}


shinyServer(function(input, output, session){
  
  observe({
    # user chooses the crop as the selected input
    x <- input$inSelectedCrop
    
    filtered_CWRs <- filter(full_gap_table, full_gap_table$crop == x)
    
    # update select input so that CWRs choices are only those within the specified Crop
    # order filtered table so that user choices for CWR are alphabetically organized
    filtered_CWRs <- filtered_CWRs[order(filtered_CWRs$species),]
    
    updateSelectInput(session, "inSelectedCWR",
                      label = paste("Select a Crop Wild Relative"),
                      choices = filtered_CWRs$species
    ) # updateSelectInput
    
  }) # observe
  
  # add option for user to select province or ecoregion
  # figure out how to keep map up and show native range for CWRs with ZERO accessions
  plotData <- reactive({ 
    if(input$inProvincesOrEcoregions == "Provinces"){
      # TRUE (user inputs "Provinces")
      test <- full_gap_table %>%
        filter(full_gap_table$species == input$inSelectedCWR) %>%
        group_by(province) %>%
        # tally the number of rows in each ecoregion with an existing accession (garden is not NA)
        add_tally(!is.na(garden)) %>%
        rename("accessions_in_province" = "n")  %>%
        ungroup() %>%
        mutate(total_accessions_for_species = sum(!is.na(garden))) %>%
        mutate(accessions_no_geo_data = sum(is.na(province))) %>%
        mutate(accessions_with_geo_data = sum(!is.na(province))) %>%
        group_by(province) %>%
        filter(row_number() == 1) %>%
        filter(!is.na(province)) %>%
        # find number of accessions where province = NA and add this as a universal col
        # drop rows where province = NA
        mutate(binary = ifelse(
          accessions_in_province > 0, 1, 0)) %>%
        ungroup() %>%
        mutate(num_native_province = sum(!duplicated(province))) %>%
        mutate(num_covered_province = sum(binary)) %>%
        mutate(perc_province_range_covered = 
                 num_covered_province / num_native_province)
      
        tigris::geo_join(canada_provinces_geojson, test,  
                         by_sp = "name", by_df = "province")
    
    } else{
      # FALSE, user inputs "Ecoregions"
      test <- full_gap_table %>%
        filter(full_gap_table$species == input$inSelectedCWR) %>%
        group_by(ECO_NAME) %>%
        # tally the number of rows in each ecoregion with an existing accession (garden is not NA)
        add_tally(!is.na(garden)) %>%
        rename("accessions_in_ecoregion" = "n")  %>%
        ungroup() %>%
        mutate(total_accessions_for_species = sum(!is.na(garden))) %>%
        mutate(accessions_no_geo_data = sum(is.na(ECO_NAME))) %>%
        mutate(accessions_with_geo_data = sum(!is.na(ECO_NAME))) %>%
        group_by(ECO_NAME) %>%
        filter(row_number() == 1) %>%
        filter(!is.na(ECO_NAME)) %>%
        # find number of accessions where ECO_NAME = NA and add this as a universal col
        # drop rows where ECO_NAME = NA
        mutate(binary = ifelse(
          accessions_in_ecoregion > 0, 1, 0)) %>%
        ungroup() %>%
        mutate(num_native_ecoregions = sum(!duplicated(ECO_NAME))) %>%
        mutate(num_covered_ecoregions = sum(binary)) %>%
        mutate(perc_ecoregion_range_covered = 
                 num_covered_ecoregions / num_native_ecoregions) 
      
        tigris::geo_join(canada_ecoregions_geojson, test, by = "ECO_CODE")
    } # else
    
  }) # reactive
  
  tableData <- reactive({ 
    
    if(input$inProvincesOrEcoregions == "Provinces"){
      
      test <- full_gap_table %>%
        # filter the table to selected CWR
        filter(full_gap_table$species == input$inSelectedCWR) %>%
        
        # could use this later for a heatmap, but accessions_in_province is not being used right now
        # tally the number of rows in each province with an existing accession (garden is not NA)
        group_by(province) %>%
        add_tally(!is.na(garden)) %>%
        rename("accessions_in_province" = "n")  %>%
        ungroup() %>%
        

        # filter table to accessions only, other rows represent 
        # and then add all the accessions 
        
        
        mutate(accessions_no_geo_data = sum(is.na(province))) %>%
        # sume rows where garden is not 
        mutate(accessions_with_geo_data = sum(!is.na(province) & !is.na(garden)))  %>%
        # mutate(accessions_with_geo_data = sum(!is.na(province))) %>%
        mutate(total_accessions_for_species = accessions_with_geo_data + accessions_no_geo_data) %>%
        
        group_by(province) %>%
        filter(row_number() == 1) %>%
        filter(!is.na(province)) %>%
        # find number of accessions where province = NA and add this as a universal col
        # drop rows where province = NA
        mutate(binary = ifelse(
          accessions_in_province > 0, 1, 0)) %>%
        ungroup() %>%
        mutate(num_native_province = sum(!duplicated(province))) %>%
        mutate(num_covered_province = sum(binary)) %>%
        mutate(perc_province_range_covered = 
                 num_covered_province / num_native_province) %>%
        filter(row_number() == 1) %>%
        dplyr::select(species, crop, num_native_province, num_covered_province,
                      accessions_with_geo_data, accessions_no_geo_data, total_accessions_for_species) %>%
        mutate(num_covered_province = as.integer(num_covered_province)) %>%
        rename("native provinces" = num_native_province,
               "covered provinces" = num_covered_province,
               "accessions with geographic data" = accessions_with_geo_data,
               "accessions lacking geographic data" = accessions_no_geo_data,
               "total accessions" = total_accessions_for_species)
        
      
    } else {
      test <- full_gap_table %>%
        # filter the table to selected CWR
        filter(full_gap_table$species == input$inSelectedCWR) %>%
        
        # could use this later for a heatmap, but accessions_in_ecoregion is not being used right now
        # tally the number of rows in each ECO_NAME with an existing accession (garden is not NA)
        group_by(ECO_NAME) %>%
        add_tally(!is.na(garden)) %>%
        rename("accessions_in_ecoregion" = "n")  %>%
        ungroup() %>%
        
        
        # filter table to accessions only, other rows represent 
        # and then add all the accessions 
        
        
        mutate(accessions_no_geo_data = sum(is.na(ECO_NAME))) %>%
        # sume rows where garden is not 
        mutate(accessions_with_geo_data = sum(!is.na(ECO_NAME) & !is.na(garden)))  %>%
        # mutate(accessions_with_geo_data = sum(!is.na(ECO_NAME))) %>%
        mutate(total_accessions_for_species = accessions_with_geo_data + accessions_no_geo_data) %>%
        
        group_by(ECO_NAME) %>%
        filter(row_number() == 1) %>%
        filter(!is.na(ECO_NAME)) %>%
        # find number of accessions where ECO_NAME = NA and add this as a universal col
        # drop rows where ECO_NAME = NA
        mutate(binary = ifelse(
          accessions_in_ecoregion > 0, 1, 0)) %>%
        ungroup() %>%
        mutate(num_native_ecoregions = sum(!duplicated(ECO_NAME))) %>%
        mutate(num_covered_ecoregions = sum(binary)) %>%
        mutate(perc_ecoregion_range_covered = 
                 num_covered_ecoregions / num_native_ecoregions) %>%
        filter(row_number() == 1) %>%
        dplyr::select(species, crop, num_native_ecoregions, num_covered_ecoregions,
                      accessions_with_geo_data, accessions_no_geo_data, total_accessions_for_species) %>%
        mutate(num_covered_ecoregions = as.integer(num_covered_ecoregions)) %>%
        rename("native ecoregions" = num_native_ecoregions,
               "covered ecoregions" = num_covered_ecoregions,
               "accessions with geographic data" = accessions_with_geo_data,
               "accessions lacking geographic data" = accessions_no_geo_data,
               "total accessions" = total_accessions_for_species)
    }
    
      
  })
  
    # update so that the main panel has a message e.g. choose a CWR
    # update so that Species X is replaced by input$inSelectedCWR
    # update so that no data is still blue rather than yellow when there's no accessions with geo data
    # add a legend
    output$gapPlot <- renderPlot({
        
        ggplot(plotData()) +
        geom_sf(aes(fill = binary),
          color = "gray60", size = 0.1) +
        coord_sf(crs = crs_string) +
        scale_fill_distiller(palette = "Spectral") +
        guides(fill = FALSE) +
        theme_map() +
        ggtitle("") +
        theme(panel.grid.major = element_line(color = "white"),
              legend.key = element_rect(color = "gray40", size = 0.1),
              plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5)
        )
      
    }) # renderPlot
  
    output$gapTable <- renderTable({
      tableData()
    }) # renderTable
    
}) # server