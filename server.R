library(tidyverse)
library(shiny)

tibGardens <- as_tibble(read.csv("Gardens.csv"))

shinyServer(function(input, output){
  
  # gardens is a test output
  output$gardenData <- renderTable({
    provinceFilter <- subset(tibGardens, tibGardens$Province == input$inProvince)
  })
  
  # output$_ <- render_({})

})


