library(shiny)
library(tidyverse)
library(plotly)

ui <- fluidPage(
  titlePanel(title = "Sample size calculator"),
  
  p("Calculate sample size as a function of reliability requirements for 
    categorical measurements. This tool also lets a few failure be taken into 
    account."),
  
  fluidRow(
    column(3,
           wellPanel(
             p("Enter parameters"),
             
             numericInput(inputId = 'cl', '
                          Confidence level', 
                          value = 0.95, 
                          step = 0.01
             ),
             
             selectInput("failures", 
                         "Up to how many failures:", 
                         c(0,1,2,3)
             )
           )
    ),
    
    column(9,
           plotlyOutput("p")
    )
  )
)
