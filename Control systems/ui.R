library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel(title = "System response to unit step with PID controller"),
  p("A hypothetical control system with adjustable parameters"),
  
  fluidRow(
    column(3,
           wellPanel(
             p("Adjust parameters and see the effect on the graph"),
             numericInput(inputId = 'kp', 'Kp', value = 0.04, step = 0.01),
             numericInput(inputId = 'ti_inverse', '1/Ti', value = 0.007, step = .001),
             numericInput(inputId = 'td', 'Td', value = 6),
             numericInput(inputId = 'bias', 'Bias', value = 0.005, step = .001),
             numericInput(inputId = 'delay', 'Delay', value = 20)
           )),
    column(9,
           plotOutput("p1"))
  )
  
)