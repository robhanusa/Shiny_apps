library(shiny)
library(tidyverse)

control_response <- function(kp,td,ti_inverse,bias,delay) {
  
  derivative_smoothing = 5
  
  sp <- c(rep(0,50),rep(1,950))
  error <- c(rep(0,1000))
  cum_error <- c(rep(0,1000))
  pv <- c(rep(0,1000))
  df <- data.frame(sp,error,cum_error,pv)
  
  for (i in c(1:nrow(df))){
    if (i > 1) df[i,'pv'] <-  df[i-1,'pv'] + bias
    if (i > delay) df[i,'error'] <- df[i,'sp']-df[(i-delay),'pv']
    if (i > 1) {
      if (df[i,'sp'] != df[(i-1),'sp']) {
        df[i,'cum_error'] <- 0 #reset the cumulative error when the sp changes
      } else {
        df[i,'cum_error'] <- df[(i-1),'cum_error'] + df[i,'error']
      }
    }
    if (i <= 2*derivative_smoothing && i > 1) {
      df[i,'pv'] <-  df[i,'pv'] + kp*(df[i,'error']+ti_inverse*df[i,'cum_error'])
    } else if (i > 2*derivative_smoothing) {
      df[i,'pv'] <-  df[i,'pv'] + kp*(df[i,'error']+
                                                 ti_inverse*df[i,'cum_error']+
                                                 td*(mean(df[i:(i-derivative_smoothing),'error'])-
                                                       mean(df[(i-1-derivative_smoothing):(i-2*derivative_smoothing),'error']))/
                                                 (2*derivative_smoothing))
    }
  }
  return(df)
}

server <- function(input,output,session) {
  response <- reactive({
    control_response(input$kp,input$td,input$ti_inverse,input$bias,input$delay)
  })
  output$p1 <- renderPlot({
    ggplot(data = response(),aes(x = c(1:nrow(response()))))+
      geom_line(mapping = aes(y = pv, color = 'Process Value'))+
      geom_line(mapping = aes(y = sp, color = 'Set Point'))+
      xlab("Time") +
      ylab("Response")+
      theme(axis.title=element_text(size=14),
            axis.text=element_text(size=12),
            legend.text=element_text(size = 12),
            legend.title = element_blank())
  })
}