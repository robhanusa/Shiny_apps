library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)

#Colors----
faintRed <- 'rgba(255,0,0,.5)'
faintGreen <- 'rgba(0,255,0,.5)'

cons1_per_prod <- data.frame(prod_1 = rep(12,52))

server <- function(input,output,session){

    
  order_1 <- reactive(c('mat_1' = ifelse(input$include_order1,input$mat1_1,0),
              'week_num' = 10))
  
  lead_time <- reactive(input$lead_time)
  
  #allow toggle of all products, and to choose start week
  include_prod1 <- reactive(input$include_prod1)
  prod1_start <- reactive(input$prod1_start)
  
  #function to calculate weekly consumption. I can probably do this better with
  #lapply than using the for loop- Revisit
  calc_consumption <- function(cons_per_prod){
    cons_tot <- rep(0,52)
    for (i in 1:length(cons_tot)){
      if(include_prod1() && i >= prod1_start()){
        cons_tot[i] <- cons_per_prod[i,"prod_1"]
      }
    }
    return(reactive(cons_tot))
  }
  
  cons1_tot <- reactive(calc_consumption(cons1_per_prod))
  
  #calculate weekly stock of each material
  
  calc_stock <- function(start_stock, consumption, material){
    stock <- c(start_stock-consumption()[1], rep(0,51))
    for (i in 2:length(stock)){
      stock[i] <- stock[i-1]-consumption()[i]
      for (order in list(order_1())){
        if (order['week_num'] == i){
          stock[i] <- stock[i] + order[material] #where material <- 'mat_1' for example
        }
      }
    }
     return(stock)
  }
  
  stock1 <- reactive(calc_stock(input$mat1_start,cons1_tot(),'mat_1'))

  week_num <- reactive( seq(1,length(stock1())))
  x_axis <- reactive(rep(0,length(stock1())))
  

  #note tht the column names of the reactive elements are going to have '..' at the end
  #ex 'week_num..' But the new columns I add (ex stock1_pos) don't have these dots
  #this is important when rendering the graph
  df <- reactive(data.frame(week_num(),stock1(),x_axis()))
  #create rows where only positive or negative are present, to make the red/green ribbons
  df1 <- reactive(mutate(df(),stock1_pos = ifelse(stock1() > 0, stock1(), 0)))
  df2 <- reactive(mutate(df1(),stock1_neg = ifelse(stock1() < 0, stock1(), 0)))
  
  #vertical line function
  vline <- function(x = 0, color = "orange") {
    list(
      type = "line",
      y0 = 0,
      y1 = 1,
      yref = "paper",
      x0 = x,
      x1 = x,
      line = list(color = color, dash="solid")
    )
  }
  
  #create list for shapes parameter in plotly layout
  include_order1 <- reactive(input$include_order1)
  
  make_vline_list <- function(include_order1,order_1,lead_time){
  vline_list <- list()
  if(include_order1){
    vline_list <- append(vline_list,list(vline(order_1['week_num']),
                                   vline(order_1['week_num'] - lead_time, color = 'red')))
  }
  }
  
  vline_list <- reactive(make_vline_list(include_order1(),order_1(),lead_time()))
  
  #create df's for text labels on vertical lines
  receive_text <- c('Receive\norder 1')
  receive_x <- reactive(c(order_1()['week_num'] + 2))
  receive_y <- reactive(c(max(stock1())))

  place_text <- c('Place\norder 1')
  place_x <- reactive(c(order_1()['week_num'] - lead_time() + 2))
  place_y <- reactive(c(max(stock1())-100))

  df_text <- reactive(data.frame(receive_text, receive_x(), receive_y(),
                        place_text, place_x(), place_y()))
  
  #graphic for material 1
  output$p1 <- renderPlotly({
   p1 <- plot_ly(df2(), x=~week_num.., y=~stock1.., mode = 'lines',type = 'scatter',
                 line = list(color = 'grey', width = 2))
  p1 <- p1 %>% add_ribbons(ymin = ~x_axis..,
                           ymax = ~stock1_pos,
                           line = list(color = 'black', width = 0),
                           fillcolor = faintGreen)
  p1 <- p1 %>% add_ribbons(ymin = ~stock1_neg,
                           ymax = ~x_axis..,
                           line = list(color = 'black', width = 0),
                           fillcolor = faintRed)
  #add vertical lines and format layout
  p1 <- p1 %>% layout(showlegend = FALSE,
                      yaxis = list(title = ''),
                      xaxis = list(title = 'Week'))
  p1 <- p1 %>% layout(shapes = vline_list())
  #add text labels on 'receive' vertical lines
  p1 <- p1 %>% add_trace(data = df_text(), x = ~receive_x.., y = ~receive_y..,
                         type = 'scatter', mode = 'text', text = ~receive_text,
                         line = NULL)
  #add text labels on 'place' vertical lines
  p1 <- p1 %>% add_trace(data = df_text(), x = ~place_x.., y = ~place_y..,
                         type = 'scatter', mode = 'text', text = ~place_text,
                         line = NULL)

  return(p1)
})
}