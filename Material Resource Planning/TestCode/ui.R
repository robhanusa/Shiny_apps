library(shiny)
library(tidyverse)
library(plotly)

ui <- fluidPage(
  titlePanel(title = "MRP"),
  fluidRow(
    column(3,
           wellPanel(
             #fileInput("upload"), #this line is missing a label and will crash the app
             fluidRow(
               column(5,
                checkboxInput(inputId = 'include_prod1', 'Include Product 1?',
                              value = TRUE)),
              column(7,
                numericInput(inputId = 'prod1_start', 'Starting week, Product 1', 
                          value = 12, step = 1))),
             fluidRow(
               column(6,
                   numericInput(inputId = 'mat1_start', 'Starting stock, Material 1', 
                                value = 200, step = 1)),
              ),
             numericInput(inputId = 'lead_time', 'Lead time, in weeks', 
                          value = 4, step = 1),
             checkboxInput(inputId = 'include_order1', 'Include Order 1?',
                           value = TRUE),
             fluidRow(
               column(6,
                      numericInput(inputId = 'mat1_1', 'Material 1, Order 1',
                                   value = 25, step = 1)),
               ),
           )),
    column(9,
           plotlyOutput("p1"),
           verbatimTextOutput("event")
           #plotlyOutput("p2"))
  )
  
)
)