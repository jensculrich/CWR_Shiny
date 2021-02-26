
library(tidyverse)
library(shiny)

tibGardens <- as_tibble(read.csv("Gardens.csv"))

shinyServer(function(input, output){
  output$gardenData <- renderTable({
    provinceFilter <- subset(tibGardens, tibGardens$Province == input$inProvince)
  })

})


