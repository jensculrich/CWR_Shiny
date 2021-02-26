
library(tidyverse)
library(shiny)

tib <- as_tibble(read.csv("Gardens.csv"))

shinyServer(function(input, output){
  output$gardenData <- renderTable({
    provinceFilter <- subset(tib, tib$Province == input$inProvince)
  })

})


