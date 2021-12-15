library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)

#Colors----
faintRed <- 'rgba(255,0,0,.5)'
faintGreen <- 'rgba(0,255,0,.5)'

#material consumption per product by week of material 1. Eventually I'll
#make this a file upload
cons1_per_prod <- data.frame(prod_1 = rep(12,52),
                             prod_2 = rep(c(12,9),26),
                             prod_3 = rep(28,52))

#material consumption per product by week of material 2
cons2_per_prod <- data.frame(prod_1 = rep(0,52),
                             prod_2 = rep(c(4,3),26),
                             prod_3 = rep(35,52))

server <- function(input,output,session){

  #Toggle products----
  #have "product 1 start" week box not show up if 'include prod 1' is unchecked
  observeEvent(input$include_prod1,{
    toggle("prod1_start")
  },
  ignoreInit = TRUE)
  
  observeEvent(input$include_prod2,{
    toggle("prod2_start")
  },
  ignoreInit = TRUE)
  
  observeEvent(input$include_prod3,{
    toggle("prod3_start")
  },
  ignoreInit = TRUE)
  
  #Toggle order inclusion----
  #have 'material 1, order 1' not appear when 'include Order 1' is unchecked
  observeEvent(input$include_order1,{
    toggle("mat1_1")
    toggle("mat2_1")
  },
  ignoreInit = TRUE)
  
  observeEvent(input$include_order2,{
    toggle("mat1_2")
    toggle("mat2_2")
  },
  ignoreInit = TRUE)
  
  observeEvent(input$include_order3,{
    toggle("mat1_3")
    toggle("mat2_3")
  },
  ignoreInit = TRUE)
  
  order_1 <- reactive(c('mat_1' = ifelse(input$include_order1,input$mat1_1,0),
               'mat_2' = ifelse(input$include_order1,input$mat2_1,0),
               'week_num' = 10))
  order_2 <- reactive(c('mat_1' = ifelse(input$include_order2,input$mat1_2,0),
               'mat_2' = ifelse(input$include_order2,input$mat2_2,0),
               'week_num' = 24 ))
  order_3 <- reactive(c('mat_1' = ifelse(input$include_order3,input$mat1_3,0),
               'mat_2' = ifelse(input$include_order3,input$mat2_3,0),
               'week_num' = 34))
  
  orders <- reactive(list(order_1(),order_2(),order_3()))
  
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
      for (order in orders()){
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
  
  #Make vertical lines----
  include_orders <- reactive(list(input$include_order1,
                                  input$include_order2,
                                  input$include_order3))
  
  make_vline_list <- function(include_orders,order,lead_time){
    vline_list <- list()
      for (i in 1:length(include_orders)){
        if(include_orders[[i]]){
          vline_list <- append(vline_list,list(vline(order[[i]]['week_num']),
                                         vline(order[[i]]['week_num'] - lead_time, color = 'red')))
        }
      }
    return(vline_list)
    }
  
  vline_list <- reactive(make_vline_list(include_orders(),orders(),lead_time()))
  
  #create df's for text labels on vertical lines
  make_labels <- function (n) {
    receive_text <- c(paste0('Receive\norder ',n))
    receive_x <- reactive(c(orders()[[n]]['week_num'] + 2))
    receive_y <- reactive(c(max(stock1())))
  
    place_text <- c(paste0('Place\norder ',n))
    place_x <- reactive(c(orders()[[n]]['week_num'] - lead_time() + 2))
    place_y <- reactive(c(max(stock1())-100))
  
    return(reactive(data.frame(receive_text, receive_x(), receive_y(),
                        place_text, place_x(), place_y())))
  }
  
  df_text <- make_labels(1)
  
  #want to figure out how to add rows to df_text for the other line labels
  observe(print(df_text()))
  
  # #create df's for text labels on vertical lines
  # make_text_labels <- function() {
  #   for (i in 1:length(orders())){
  #     if (i == 1){
  #       receive_text <- c('Receive\norder ${i}')
  #       receive_x <- reactive(c(order()[i]['week_num'] + 2))
  #       receive_y <- reactive(c(max(stock1())))
  #       
  #       place_text <- c('Place\norder ${i}')
  #       place_x <- reactive(c(order()[i]['week_num'] - lead_time() + 2))
  #       place_y <- reactive(c(max(stock1())-100))
  #     } else {
  #       append(receive_text, c('Receive\norder ${i}'))
  #       append(receive_x, reactive(c(order()[i]['week_num'] + 2)))
  #       append(receive_y, reactive(c(max(stock1()))))
  #       
  #       append(place_text, c('Place\norder ${i}'))
  #       append(place_x, reactive(c(order()[i]['week_num'] - lead_time() + 2)))
  #       append(place_y, reactive(c(max(stock1())-100)))
  #       
  #     }
  #   }
  #   observe(print(receive_x()))
  #   return(data.frame(receive_text, receive_x(), receive_y(),
  #                     place_text, place_x(), place_y()))
  # }
  # 
  # df_text <- reactive(make_text_labels())
  
  #graphic for material 1
  output$p1 <- renderPlotly({
   p1 <- plot_ly(df2(), x=~week_num.., y=~stock1.., mode = 'lines',type = 'scatter',
                 line = list(color = 'grey', width = 2),
                 hovertemplate = paste(paste0('<extra></extra>Stock: %{y}\nWeek: %{x}')))
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