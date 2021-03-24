# in progress: updateSelectInput so that user Crop selection filters 
# down second selection box to only related CWRs

gardens <- as_data_frame(read.csv("Gardens.csv"))
canada_ecoregions_geojson <- st_read("canada_ecoregions_clipped.geojson", quiet = TRUE)
canada_provinces_geojson <- st_read("canada_provinces.geojson", quiet = TRUE)
full_gap_table <- as_data_frame(read.csv("full_gap_table.csv"))

crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
# Define the maps' theme -- remove axes, ticks, borders, legends, etc.
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
  
  # gardens is a test output
  output$gardenData <- renderTable({
    provinceFilter <- subset(gardens, gardens$Province == input$inProvince)
  }) # output$gardenData
  
  observe({
    # user chooses the crop as the selected input
    x <- input$inSelectedCrop
    
    filtered_CWRs <- filter(full_gap_table, full_gap_table$crop == x)
    
    # update select input so that CWRs choices are only those within the specified Crop
    updateSelectInput(session, "inSelectedCWR",
                      label = paste("Select a Crop Wild Relative"),
                      choices = filtered_CWRs$species
    ) # updateSelectInput
    
  }) # observe
  
  dat <- reactive({ 
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
                 num_covered_ecoregions / num_native_ecoregions) %>%
        dplyr::select(-country, -geometry, -latitude, -longitude, 
                      -garden, -X, -province)
        
       tigris::geo_join(canada_ecoregions_geojson, test, by = "ECO_NAME")
       }) # reactive
  
      output$gapAnalysis <- renderPlot({
        ggplot(dat()) +
        geom_sf(aes(fill = binary),
          color = "gray60", size = 0.1) +
        coord_sf(crs = crs_string) +
        scale_fill_distiller(palette = "Spectral") +
        guides(fill = FALSE) +
        theme_map() +
        ggtitle("Conservation of Species X Across Native Range in Canadian Botanic Gardens") +
        theme(panel.grid.major = element_line(color = "white"),
              legend.key = element_rect(color = "gray40", size = 0.1),
              plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5)
        )
      
    }) # renderPlot
  
}) # server
  

  # example code from local analysis 
  #species_gap_analyis_by_ecoregion_sf <- tigris::geo_join(canada_eco_subset, species_gap_analyis_by_ecoregion, by = "ECO_NAME")
  
  #ZZ <- ggplot() +
  #  geom_sf(
  #    aes(fill = binary), 
  #    color = "gray60", size = 0.1, data = species_gap_analyis_by_ecoregion_sf) +
  #  coord_sf(crs = crs_string) +
  #  scale_fill_distiller(palette = "Spectral") +
  #  guides(fill = FALSE) +
  #  theme_map() +
  #  ggtitle("Conservation of Species X Across Native Range in Canadian Botanic Gardens") +
  #  theme(panel.grid.major = element_line(color = "white"),
  #        legend.key = element_rect(color = "gray40", size = 0.1),
  #        plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5)
  #  )
  #ZZ



