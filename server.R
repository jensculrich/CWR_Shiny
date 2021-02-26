library(tidyverse)
library(shiny)

tib <- as_tibble(read.csv("Gardens.csv"))

server <- function(input, output){
  
  tab <- reactive({ # <-- Reactive function here, filter table according to input
    tib %>% 
      filter(Province == input$var1)
  })
  
  output$table <- renderTable({ # display filtered table
      tab()
    }
  )
}