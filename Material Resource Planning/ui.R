library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(shinyjs)

ui <- dashboardPage(
  
  # Header ----
  dashboardHeader(
    title = "Material Resource Planning",
    titleWidth = 300
  ),
  
  dashboardSidebar(
    width = 300,
    
    # Date range ----
    fluidRow(
      column(
        width = 12,
        dateRangeInput(
          inputId = "date_range",
          label = "Select Date Range:",
          start = Sys.Date(),
          end   = Sys.Date() + 360
        )
      )
    ),
  
    # Material starting stocks ----
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = 'mat1_start', 
          div('Starting stock, Material 1'), 
          value = 200, 
          step = 1
        )
      ),
     
      column(
        width = 6,
        numericInput(
          inputId = 'mat2_start', 
          'Starting stock, Material 2', 
          value = 100, 
          step = 1
        )
      )
    ),
  
    # Starting date or consumption of product 1 ----
    fluidRow(
      # useShinyjs() is needed to allow us to toggle in observeEvent in server side
      useShinyjs(),
      column(
        width = 6,
        checkboxInput(
          inputId = 'include_prod1', 
          'Include Product 1?',
          value = TRUE
        )
      ),
      
      column(
        width = 6,
        dateInput(
          'prod1_startDate', 
          'Starting date', 
          value = Sys.Date() + 60
        )
      )
    ),
  
    # Starting date or consumption of product 2 ----
    fluidRow(
      column(
        width = 6,
        checkboxInput(
          inputId = 'include_prod2', 
          'Include Product 2?',
          value = TRUE
        )
      ),
      
      column(
        width = 6,
        dateInput(
          'prod2_startDate', 
          'Starting date', 
          value = Sys.Date() + 120
        )
      )
    ),
  
    # Starting date or consumption of product 3 ----
    fluidRow(
      column(
        width = 6,
        checkboxInput(
          inputId = 'include_prod3', 
          'Include Product 3?',
          value = TRUE
        )
      ),
      
      column(
        width = 6,
        dateInput(
          'prod3_startDate', 
          'Starting date',
          value = Sys.Date() + 360
        )
      )
    ),
  
    # Lead time ----
    numericInput(
      inputId = 'lead_time', 
      'Lead time, in weeks', 
      value = 4, 
      step = 1
    ),

    # Order 1 ----
    checkboxInput(
      inputId = 'include_order1', 
      'Include Order 1?',
      value = TRUE
    ),
  
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = 'mat1_1', 
          'Material 1, Order 1',
          value = 200, 
          step = 1
        )
      ),
      
      column(
        width = 6,
        numericInput(
          inputId = 'mat2_1', 
          'Material 2, Order 1',
          value = 200, 
          step = 1
        )
      ),
      
      column(
        width = 12,
        dateInput(
          'order1_arrivalDate', 
          'Arrival date', 
          value = Sys.Date() + 60
        )
      )
    ),

    # Order 2 ----
    checkboxInput(
      inputId = 'include_order2', 
      'Include Order 2?',
      value = TRUE
    ),
  
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = 'mat1_2', 
          'Material 1, Order 2',
          value = 250, 
          step = 1
        )
      ),
      
      column(
        width = 6,
        numericInput(
          inputId = 'mat2_2', 
          'Material 2, Order 2',
          value = 250, 
          step = 1
        )
      ),
      
      column(
        width = 12,
        dateInput(
          'order2_arrivalDate', 
          'Arrival date', 
          value = Sys.Date() + 120
        )
      )
    ),
  
    # Order 3 ----
    checkboxInput(
      inputId = 'include_order3', 
      'Include Order 3?',
      value = TRUE
      ),
  
    fluidRow(
      column(
        width = 6,
        numericInput(
          inputId = 'mat1_3', 
          'Material 1, Order 3',
          value = 250, 
          step = 1
        )
      ),
      
      column(
        width = 6,
        numericInput(
          inputId = 'mat2_3', 
          'Material 2, Order 3',
          value = 250, 
          step = 1
        )
      ),
      
      column(
        width = 12,
        dateInput(
          'order3_arrivalDate', 
          'Arrival date', 
          value = Sys.Date() + 330
        )
      )
    )
  ),
  
    # Plots ----
    dashboardBody(
      tabsetPanel(
        
        # Plot for inventory of material 1 ----
        tabPanel(
          title = 'Material 1',
          plotlyOutput("p1")
        ),
        
        # Plot for inventory of material 2 ----
        tabPanel(
          title = 'Material 2',
          plotlyOutput("p2")
        )
      )
    )
  )