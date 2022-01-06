library(shiny)
library(tidyverse)
library(plotly)

#Sample size function----
samples <- function(confidence, failures, rel) {
  round((0.5*qchisq(1-confidence,
                    2*(failures+1),
                    lower.tail = F)
         /(1 - rel))
  )
}

server <- function(input,output,session) {
  output$p <- renderPlotly({
    
    #create vectors that will become x and y axes
    reliability_fraction <- seq(0.9,.999,0.001)
    Sample_size_needed_with_0_failures <- samples(input$cl,0,reliability_fraction)
    
    Reliability <- reliability_fraction*100
    
    #Make plot----
    p <- ggplot(mapping=aes(x=Reliability))+
      scale_y_log10() +
      xlab("% Reliability") +
      ylab("Samples needed")+
      theme(axis.title=element_text(size=14),
            axis.text=element_text(size=12))+
      geom_line(aes(y=Sample_size_needed_with_0_failures))
      
      #Add lines for different numbers of failures
      if (input$failures >= 1){
        Sample_size_needed_with_1_failure <- samples(input$cl,1,reliability_fraction)
        p <- p + geom_line(aes(y=Sample_size_needed_with_1_failure))
      } 
      if (input$failures >= 2){
        Sample_size_needed_with_2_failures <- samples(input$cl,2,reliability_fraction)
        p <- p + geom_line(aes(y=Sample_size_needed_with_2_failures))
      }
      if (input$failures >= 3){
        Sample_size_needed_with_3_failures <- samples(input$cl,3,reliability_fraction)
        p <- p + geom_line(aes(y=Sample_size_needed_with_3_failures))
      }
      
    return(p)
  })
}