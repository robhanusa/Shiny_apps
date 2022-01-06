library(shiny)
library(tidyverse)
library(plotly)

#UI----
ui <- fluidPage(
  titlePanel(title = "Mean time to failure calculator"),
  p("Enter cycle count for each of up to 8 samples. Indicate if the sample failed or not. If less than 8 samples were used, leave the remaining boxes at 0 cycles and no failures"),
  fluidRow(
    column(3,
           wellPanel(
             numericInput(inputId = 'samples_1', 'Sample 1 cycles:', value = 0),
             checkboxInput("samples_failed_1", value = FALSE, label = "Check if failed"),
             numericInput(inputId = 'samples_2', 'Sample 2 cycles:', value = 0),
             checkboxInput("samples_failed_2", value = FALSE, label = "Check if failed"),
             numericInput(inputId = 'samples_3', 'Sample 3 cycles:', value = 0),
             checkboxInput("samples_failed_3", value = FALSE, label = "Check if failed"),
             numericInput(inputId = 'samples_4', 'Sample 4 cycles:', value = 0),
             checkboxInput("samples_failed_4", value = FALSE, label = "Check if failed"),
             numericInput(inputId = 'samples_5', 'Sample 5 cycles:', value = 0),
             checkboxInput("samples_failed_5", value = FALSE, label = "Check if failed"),
             numericInput(inputId = 'samples_6', 'Sample 6 cycles:', value = 0),
             checkboxInput("samples_failed_6", value = FALSE, label = "Check if failed"),
             numericInput(inputId = 'samples_7', 'Sample 7 cycles:', value = 0),
             checkboxInput("samples_failed_7", value = FALSE, label = "Check if failed"),
             numericInput(inputId = 'samples_8', 'Sample 8 cycles:', value = 0),
             checkboxInput("samples_failed_8", value = FALSE, label = "Check if failed")
             )),
    column(9,
           plotlyOutput("p", height = "700px"))
  )
  
)

#Calculate MTTF (Mean time to failure)----
calc_mttf <- function(num_cycles_per_sample,cl,failures){
  num_cycles <- sum(num_cycles_per_sample)
  num_failures <- sum(failures)
  mttf <- round(2*num_cycles/qchisq(1-cl/100,2*(num_failures+1),lower.tail = F))
  return(mttf)
}

#Server----
server <- function(input,output,session) {
  output$p <- renderPlotly({
    confidence <- seq(50,99.9,0.1)
    
    samples <- c(input$samples_1,
                 input$samples_2,
                 input$samples_3,
                 input$samples_4,
                 input$samples_5,
                 input$samples_6,
                 input$samples_7,
                 input$samples_8)
    samples_failed <- c(input$samples_failed_1,
                        input$samples_failed_2,
                        input$samples_failed_3,
                        input$samples_failed_4,
                        input$samples_failed_5,
                        input$samples_failed_6,
                        input$samples_failed_7,
                        input$samples_failed_8)
    
    #Make plot----
    p <- plot_ly(x = ~confidence,
                 y = ~calc_mttf(samples,confidence,samples_failed),
                 hovertemplate = paste('Confidence level: %{x:.1f}%',
                                       '<br>Number of cycles: %{y}',
                                       '<extra></extra>'),
                 showlegend = FALSE)%>%
      add_lines() %>%
      #Add 95% Confidence indicator----
      layout(xaxis = list(title="Confidence level, %"),
             yaxis = list(title="Mean time to failure, cycles"))%>%
      add_trace(x = 95,
                y = calc_mttf(samples,95,samples_failed),
                type = 'scatter',
                mode = 'markers', 
                marker = list(color = 'rgb(242,142,43)', 
                              size = 8)) 
  })
}

shinyApp(ui = ui, server = server)
  