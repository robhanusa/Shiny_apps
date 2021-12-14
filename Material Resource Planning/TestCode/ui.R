library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(shinyjs)

ui <- dashboardPage(
  dashboardHeader(title = "Material Resource Planning",
                  titleWidth = 300),
  dashboardSidebar(
    width = 300,
   #fileInput("upload"), #this line is missing a label and will crash the app
    fluidRow(
     column(6,
         numericInput(inputId = 'mat1_start', 'Starting stock, Material 1', 
                      value = 200, step = 1)),
            ),
   fluidRow(
     #useShinyjs() is needed to allow us to toggle in observeEvent in server side
     useShinyjs(),
     column(6,
      checkboxInput(inputId = 'include_prod1', 'Include Product 1?',
                    value = TRUE, )),
    column(6,
      numericInput(inputId = 'prod1_start', 'Starting week', 
                value = 12, step = 1))
          ),
   numericInput(inputId = 'lead_time', 'Lead time, in weeks', 
                value = 4, step = 1),
   checkboxInput(inputId = 'include_order1', 'Include Order 1?',
                 value = TRUE),
   fluidRow(
     column(6,
            numericInput(inputId = 'mat1_1', 'Material 1, Order 1',
                         value = 25, step = 1))
     )
 ),
    #Plots----
    dashboardBody(
      tabsetPanel(
        type = 'tabs',
        id = 'tab_selected',
        tabPanel(
          title = 'Material 1',
           plotlyOutput("p1"),
           verbatimTextOutput("event")
        ),
        tabPanel(
          title = 'Material 2'
           #plotlyOutput("p2"))
        )
  ))
  
)
