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
               column(5,
                checkboxInput(inputId = 'include_prod2', 'Include Product 2?',
                              value = TRUE)),
               column(7,
                numericInput(inputId = 'prod2_start', 'Starting week, Product 2', 
                          value = 25, step = 1))),
             fluidRow(
               column(5,
                checkboxInput(inputId = 'include_prod3', 'Include Product 3?',
                              value = TRUE)),
               column(7,
             numericInput(inputId = 'prod3_start', 'Starting week, Product 3', 
                          value = 35, step = 1))),
             fluidRow(
               column(6,
                   numericInput(inputId = 'mat1_start', 'Starting stock, Material 1', 
                                value = 200, step = 1)),
               column(6,
                   numericInput(inputId = 'mat2_start', 'Starting stock, Material 2', 
                                value = 100, step = 1))),
             numericInput(inputId = 'lead_time', 'Lead time, in weeks', 
                          value = 4, step = 1),
             checkboxInput(inputId = 'include_order1', 'Include Order 1?',
                           value = TRUE),
             fluidRow(
               column(6,
                      numericInput(inputId = 'mat1_1', 'Material 1, Order 1',
                                   value = 25, step = 1)),
               column(6,
                      numericInput(inputId = 'mat2_1', 'Material 2, Order 1',
                                   value = 25, step = 1))),
             checkboxInput(inputId = 'include_order2', 'Include Order 2?',
                           value = TRUE),
             fluidRow(
               column(6,
                      numericInput(inputId = 'mat1_2', 'Material 1, Order 2',
                                   value = 250, step = 1)),
               column(6,
                      numericInput(inputId = 'mat2_2', 'Material 2, Order 2',
                                   value = 250, step = 1))),
             checkboxInput(inputId = 'include_order3', 'Include Order 3?',
                           value = TRUE),
             fluidRow(
               column(6,
                      numericInput(inputId = 'mat1_3', 'Material 1, Order 3',
                                   value = 250, step = 1)),
               column(6,
                      numericInput(inputId = 'mat2_3', 'Material 2, Order 3',
                                   value = 250, step = 1)))
           )),
    column(9,
           plotlyOutput("p1"),
           verbatimTextOutput("event")
           #plotlyOutput("p2"))
  )
  
)
)