library(shiny)
library(tidyverse)
library(plotly)

samples <- function(confidence, failures, rel) {
  round((0.5*qchisq(1-confidence,
                    2*(failures+1),
                    lower.tail = F)
         /(1 - rel))
  )
}
#after I read more about plotly, I should to go the link below to get an example of 
#a draggable moving horizontal line
#https://community.rstudio.com/t/sliding-a-point-on-a-plot-rather-than-sliderinput-to-update-plot-in-shiny/16405/8

server <- function(input,output,session) {
  output$p <- renderPlotly({
    Reliability <- seq(0.9,.999,0.001)
    Sample_size_needed_with_0_failures <- samples(input$cl,0,Reliability)
    
    p <- ggplot(mapping=aes(x=Reliability*100))+
      scale_y_log10() +
      xlab("% Reliability") +
      ylab("Samples needed")+
      theme(axis.title=element_text(size=14),
            axis.text=element_text(size=12))+
      geom_line(aes(y=Sample_size_needed_with_0_failures))
    {
      if (input$failures >= 1){
        Sample_size_needed_with_1_failure <- samples(input$cl,1,Reliability)
        p <- p + geom_line(aes(y=Sample_size_needed_with_1_failure))
      } 
      if (input$failures >= 2){
        Sample_size_needed_with_2_failures <- samples(input$cl,2,Reliability)
        p <- p + geom_line(aes(y=Sample_size_needed_with_2_failures))
      }
      if (input$failures >= 3){
        Sample_size_needed_with_3_failures <- samples(input$cl,3,Reliability)
        p <- p + geom_line(aes(y=Sample_size_needed_with_3_failures))
      }
      }
    return(p)
  })
}