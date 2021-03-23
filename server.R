# in progress: updateSelectInput so that user Crop selection filters 
# down second selection box to only related CWRs

gardens <- as_data_frame(read.csv("Gardens.csv"))
canada_ecoregions_geojson <- st_read("canada_ecoregions_clipped.geojson", quiet = TRUE)
canada_provinces_geojson <- st_read("canada_provinces.geojson", quiet = TRUE)
full_gap_table <- as_data_frame(read.csv("full_gap_table.csv"))


shinyServer(function(input, output, session){
  
  # gardens is a test output
  output$gardenData <- renderTable({
    provinceFilter <- subset(gardens, gardens$Province == input$inProvince)
  })
  
  #observe({
  #  x <- input$inSelectedCrop
    
  #  filtered_CWRs <- filter(full_gap_table, full_gap_table$crop == x)
    
    # Can also set the label and select items
  #  updateSelectInput(session, "inSelectedCWR",
  #                    label = paste("Select a Crop Wild Relative"),
   #                   choices = filtered_CWRs$species
  #  )
 # })# observe
  
  # output$_ <- render_({})
  
  # example code from local analysis 
  
  #species_gap_analyis_by_ecoregion <- full_gap_table %>%
  # 
  #filter(species == "user input") %>%
  #group_by(ECO_NAME) %>%
  # tally the number of rows in each ecoregion with an existing accession (garden is not NA)
  #add_tally(!is.na(garden)) %>%
  #rename("accessions_in_ecoregion" = "n")  %>%
  #ungroup() %>%
  # maybe add an if else statement here, so if there's either zero accessions or at least 1?
  #mutate(total_accessions_for_species = sum(!is.na(garden))) %>%
  #mutate(accessions_no_geo_data = sum(is.na(ECO_NAME))) %>%
  #mutate(accessions_with_geo_data = sum(!is.na(ECO_NAME))) %>%
  #group_by(ECO_NAME) %>%
  #filter(row_number() == 1) %>%
  #filter(!is.na(ECO_NAME)) %>%
  # find number of accessions where ECO_NAME = NA and add this as a universal col
  # drop rows where ECO_NAME = NA
  #mutate(binary = ifelse(
  #  accessions_in_ecoregion > 0, 1, 0)) %>%
  #ungroup() %>%
  #mutate(num_native_ecoregions = sum(!duplicated(ECO_NAME))) %>%
  #mutate(num_covered_ecoregions = sum(binary)) %>%
  #mutate(perc_ecoregion_range_covered = 
  #         num_covered_ecoregions / num_native_ecoregions) %>%
  #dplyr::select(-country, -geometry, -latitude, -longitude, 
  #              -garden, -X, -province)
  
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
})


