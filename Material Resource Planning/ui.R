library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(shinyjs)

ui <- dashboardPage(

  dashboardHeader(title = "Material Resource Planning",
                  titleWidth = 300),
  dashboardSidebar(
    #import css styles sheet
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    width = 300,
    #Material starting stocks----
      fluidRow(
         column(6,
             numericInput(inputId = 'mat1_start', 
                          div('Starting stock, Material 1'), 
                          value = 200, step = 1)),
         column(6,
                numericInput(inputId = 'mat2_start', 'Starting stock, Material 2', 
                             value = 100, step = 1))
        ),
    #Product starting weeks----
   fluidRow(
     #useShinyjs() is needed to allow us to toggle in observeEvent in server side
     useShinyjs(),
     column(6,
      checkboxInput(inputId = 'include_prod1', 'Include Product 1?',
                    value = TRUE, )),
    column(6,
      dateInput('prod1_startDate', 'Starting date', value = Sys.Date()+60))
          ),
   fluidRow(
     useShinyjs(),
     column(6,
            checkboxInput(inputId = 'include_prod2', 'Include Product 2?',
                          value = TRUE, )),
     column(6,
            dateInput('prod2_startDate', 'Starting date', value = Sys.Date()+120))
    ),
   fluidRow(
     useShinyjs(),
     column(6,
            checkboxInput(inputId = 'include_prod3', 'Include Product 3?',
                          value = TRUE, )),
     column(6,
            dateInput('prod3_startDate', 'Starting date',value = Sys.Date()+360))
   ),
   numericInput(inputId = 'lead_time', 'Lead time, in weeks', 
                value = 4, step = 1),
   checkboxInput(inputId = 'include_order1', 'Include Order 1?',
                 value = TRUE),
   fluidRow(
     column(6,
            numericInput(inputId = 'mat1_1', 'Material 1, Order 1',
                         value = 200, step = 1)),
     column(6,
            numericInput(inputId = 'mat2_1', 'Material 2, Order 1',
                         value = 200, step = 1)),
     column(12,
            dateInput('order1_arrivalDate', 'Arrival date', value = Sys.Date()+60))
     ),
   checkboxInput(inputId = 'include_order2', 'Include Order 2?',
                 value = TRUE),
   fluidRow(
     column(6,
            numericInput(inputId = 'mat1_2', 'Material 1, Order 2',
                         value = 250, step = 1)),
     column(6,
            numericInput(inputId = 'mat2_2', 'Material 2, Order 2',
                         value = 250, step = 1)),
     column(12,
            dateInput('order2_arrivalDate', 'Arrival date', value = Sys.Date()+120))
     ),
   checkboxInput(inputId = 'include_order3', 'Include Order 3?',
                 value = TRUE),
   fluidRow(
     column(6,
            numericInput(inputId = 'mat1_3', 'Material 1, Order 3',
                         value = 250, step = 1)),
     column(6,
            numericInput(inputId = 'mat2_3', 'Material 2, Order 3',
                         value = 250, step = 1)),
     column(12,
            dateInput('order3_arrivalDate', 'Arrival date', value = Sys.Date()+330))
     
     )
 ),
    #Plots----
    dashboardBody(
      tabsetPanel(
        #type = 'tabs',
        #id = 'tab_selected',
        tabPanel(
          title = 'Material 1',
           plotlyOutput("p1")
           #verbatimTextOutput("event")
        ),
        tabPanel(
          title = 'Material 2',
          plotlyOutput("p2")
          #verbatimTextOutput("event")
        )
  ))
  
)

#for collapseable slidebar items: https://stackoverflow.com/questions/49852818/collapsible-menu-item-in-shiny-dashboard-sidebar