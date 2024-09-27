library(shiny)
library(tidyverse)

# Calculates a system's response to a unit change in a set point and outputs
# a dataframe with both set point and response as a function of time
control_response <- function(kp, td, ti_inverse, bias, delay) {
  
  derivative_smoothing <- 5
  
  set_point <- c(rep(0, 50), rep(1, 950))  # Set point with step change
  error <- numeric(1000)
  cum_error <- 0  # Cumulative error
  pv <- numeric(1000)  # Process value, i.e. response
  df <- data.frame(set_point, error, pv)
  
  # Loop through time horizon, ignoring 1st period as there is no feedback yet
  for (i in seq_len(nrow(df))[-1]){

    # Calculate error between set point and last measurement of process value
    if (i > delay) {
      df[i, 'error'] <- df[i, 'set_point'] - df[(i - delay), 'pv']
    }
    
    # Calculate cumulative error, resetting when set point changes
    if (df[i, 'set_point'] != df[(i - 1), 'set_point']) {
      cum_error <- 0
    } else {
      cum_error <- cum_error + df[i, 'error']
    }
    
    # Calculate process value
    pv_biased <- df[i-1, 'pv'] + bias
  
    if (i <= 2 * derivative_smoothing) {
      df[i, 'pv'] <- pv_biased + kp * (df[i, 'error'] + ti_inverse * cum_error)
    } else {
      df[i, 'pv'] <- pv_biased + kp * (
        df[i, 'error'] +
        ti_inverse * cum_error +
        td * (
          mean(df[i:(i - derivative_smoothing), 'error']) -
          mean(df[(i - 1 - derivative_smoothing):(i - 2 * derivative_smoothing), 'error'])
        ) / (2 * derivative_smoothing)
      )
    }
  }
  
  return(df)
}

server <- function(input, output, session) {
  
  response <- reactive({
    control_response(input$kp, input$td, input$ti_inverse, input$bias, input$delay)
  })
  
  output$p1 <- renderPlot({
    ggplot(data = response(), aes(x = seq_len(nrow(response())))) +
      geom_line(mapping = aes(y = pv, color = 'Process Value')) +
      geom_line(mapping = aes(y = set_point, color = 'Set Point')) +
      xlab('Time') +
      ylab('Response') +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12),
            legend.title = element_blank()
      )
  })
}