library(tidyverse)
library(shiny)

server <- function(input, output){
  output$hist <- renderPlot({ 
    hist(rnorm(input$num)) })
}