library(shiny)
library(tidyverse)
library(plotly)

ui <- fluidPage(
  titlePanel(title = "Mortgage repayment graphic"),
 
  fluidRow(
    column(3,
           wellPanel(
             numericInput(inputId = 'start_balance', 'Amount in thousands EUR', 
                          value = 100, step = 5),
             numericInput(inputId = 'term_yr', 'Loan term in years', 
                          value = 10, step = 5),
             numericInput(inputId = 'mortgage_rate_yr', 'Mortgage APR', 
                          value = 1.5, step = .1),
             numericInput(inputId = 'early_penalty', 'Early repayment penalty in %', 
                          value = 5, step = .1),
             numericInput(inputId = 'monthly_budget', 'Monthly budget in EUR', 
                          value = 1000, step = 10),
             checkboxInput(inputId = 'include_market_return', 'Compare to market return?'),
             numericInput(inputId = 'market_rate_yr', 'Market APY', 
                          value = 10, step = 1)
           )),
    column(7,
           plotlyOutput("p")),
    column(2,
           plotlyOutput("p2"))
  )

)
