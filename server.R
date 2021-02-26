library(tidyverse)
library(shiny)

tibGardens <- as_tibble(read.csv("Gardens.csv"))
tibCropMaster <- as_tibble(read.csv("CWR_Master_list.csv"))

shinyServer(function(input, output){
  output$gardenData <- renderTable({
    provinceFilter <- subset(tibGardens, tibGardens$Province == input$inProvince)
  })
  
  output$cropData <- renderTable({
    provinceFilter <- subset(tibCropMaster, tibCropMaster$Crop == input$selectedCrop)
  })

})


