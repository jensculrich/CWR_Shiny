library(tidyverse)
library(shiny)

tib <- as_tibble(read.csv("Gardens.csv"))

server <- function(input, output){
#  output$hist <- renderPlot({ 
#    hist(rnorm(input$num)) })
  tab <- reactive({ # <-- Reactive function here
    tib %>% 
      filter(Province == input$var1)
  })
  
  output$table <- renderTable({
      tab()
    }
  )
}