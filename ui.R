library(tidyverse)
library(shiny)

ui <- fluidPage(
  # *Input functions
  sliderInput(inputId = "num",
              label = "choose a number",
              value = 25, min = 1, max = 100),
  # *Output functions
  plotOutput("hist")
)
