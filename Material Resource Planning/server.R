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
  
  #Orders list----
  
  order_1 <- reactive(c('mat_1' = ifelse(input$include_order1,input$mat1_1,0),
               'mat_2' = ifelse(input$include_order1,input$mat2_1,0),
               'week_num' = input$order1_arrival))
  order_2 <- reactive(c('mat_1' = ifelse(input$include_order2,input$mat1_2,0),
               'mat_2' = ifelse(input$include_order2,input$mat2_2,0),
               'week_num' = input$order2_arrival ))
  order_3 <- reactive(c('mat_1' = ifelse(input$include_order3,input$mat1_3,0),
               'mat_2' = ifelse(input$include_order3,input$mat2_3,0),
               'week_num' = input$order3_arrival))
  
  orders <- reactive(list(order_1(),order_2(),order_3()))
  
  lead_time <- reactive(input$lead_time)
  
  #allow toggle of all products, and to choose start week
  include_prods <- reactive(c(input$include_prod1,
                             input$include_prod2,
                             input$include_prod3))
  prod_starts <- reactive(c(input$prod1_start,
                            input$prod2_start,
                            input$prod3_start))
  
  #function to calculate weekly consumption. I can probably do this better with
  #lapply than using the for loop- Revisit
  calc_consumption <- function(cons_per_prod){
    cols <- colnames(cons_per_prod)
    cons_tot <- rep(0,52)
    for (i in 1:length(cons_tot)){
      for (j in 1:length(include_prods())) {
        if(include_prods()[j] && i >= prod_starts()[j]){
          cons_tot[i] <- cons_tot[i] + cons_per_prod[i,cols[j]]
        }
      }
    }
    return(reactive(cons_tot))
  }
  
  cons_tot1 <- reactive(calc_consumption(cons1_per_prod))
  cons_tot2 <- reactive(calc_consumption(cons2_per_prod))
  
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
  
  stock1 <- reactive(calc_stock(input$mat1_start,cons_tot1(),'mat_1'))
  stock2 <- reactive(calc_stock(input$mat2_start,cons_tot2(),'mat_2'))

  week_num <- reactive( seq(1,length(stock1())))
  x_axis <- reactive(rep(0,length(stock1())))
  

  #note tht the column names of the reactive elements are going to have '..' at the end
  #ex 'week_num..' But the new columns I add (ex stock1_pos) don't have these dots
  #this is important when rendering the graph
  #dataframe for material 1----
  df_mat1 <- reactive(data.frame(week_num(),stock1(),x_axis()))
  #create rows where only positive or negative are present, to make the red/green ribbons
  df1_mat1 <- reactive(mutate(df_mat1(),stock_pos = ifelse(stock1() > 0, stock1(), 0)))
  df2_mat1 <- reactive(mutate(df1_mat1(),stock_neg = ifelse(stock1() < 0, stock1(), 0)))
 
  #dataframe for material 2----
  df_mat2 <- reactive(data.frame(week_num(),stock2(),x_axis()))
  #create rows where only positive or negative are present, to make the red/green ribbons
  df1_mat2 <- reactive(mutate(df_mat2(),stock_pos = ifelse(stock2() > 0, stock2(), 0)))
  df2_mat2 <- reactive(mutate(df1_mat2(),stock_neg = ifelse(stock2() < 0, stock2(), 0)))
  
  #Make vertical lines----
  vline <- function(x = 0, color = "orange") {
    list(
      type = "line",
      y0 = 0,
      y1 = 0.9,
      yref = "paper",
      x0 = x,
      x1 = x,
      line = list(color = color, dash="solid")
    )
  }
  
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
  
  #make text for orders----
  make_labels <- function (n, lead_time,stock) {
    #conditional is to make sure text position updates when orders() is changed
    if(!is.null(orders())) {
      receive_text <- c(paste0('Receive\norder ',n))
      receive_x <- reactive(c(orders()[[n]]['week_num']))
      receive_y <- reactive(c(max(stock)))
      
      #conditional below is needed to make sure graph reacts when lead_time is changed
      if(lead_time){
        place_text <- c(paste0('Place\norder ',n))
        place_x <- reactive(c(orders()[[n]]['week_num'] - lead_time))
        place_y <- reactive(c(max(stock)))
      }
    }
    return(reactive(data.frame(receive_text, receive_x(), receive_y(),
                        place_text, place_x(), place_y())))
  }
  
  make_df_text <- function (include_orders, lead_time, stock){
    text_list <- list()
    for (i in 1:3){
      if(include_orders()[[i]]){
        if(length(text_list) == 0) {
          text_list <- data.frame(c(make_labels(i,lead_time,stock)()))
          } else {
            text_list <- rbind(text_list,data.frame(c(make_labels(i,lead_time,stock)())))
          }
      }
    }
    return(text_list)
  }
  
  df_text_mat1 <- reactive(make_df_text(include_orders(),lead_time(),stock1()))
  df_text_mat2 <- reactive(make_df_text(include_orders(),lead_time(),stock2()))

  #format graphs----
  #function with all of the formatting that is common between graphs
  make_graph <- function (p){
    p <- p %>% add_ribbons(ymin = ~x_axis..,
                             ymax = ~stock_pos,
                             line = list(color = 'black', width = 0),
                             fillcolor = faintGreen)
    p <- p %>% add_ribbons(ymin = ~stock_neg,
                             ymax = ~x_axis..,
                             line = list(color = 'black', width = 0),
                             fillcolor = faintRed)
    #add vertical lines and format layout
    p <- p %>% layout(showlegend = FALSE,
                        yaxis = list(title = ''),
                        xaxis = list(title = 'Week'))
    p <- p %>% layout(shapes = vline_list())

    return(p)
  }
  
#Material 1 graph----
output$p1 <- renderPlotly({
  p1 <- plot_ly(df2_mat1(), x=~week_num.., y=~stock1.., mode = 'lines',type = 'scatter',
                line = list(color = 'grey', width = 2),
                hovertemplate = paste(paste0('<extra></extra>Stock: %{y}\nWeek: %{x}')))

  p1 <- make_graph(p1)
  
  #add text labels on 'receive' vertical lines
  if (length(df_text_mat1()) > 0 ) {
    p1 <- p1 %>% add_trace(data = df_text_mat1(), x = ~receive_x.., y = ~receive_y..,
                           type = 'scatter', mode = 'text', text = ~receive_text,
                           line = NULL)
    #add text labels on 'place' vertical lines
    p1 <- p1 %>% add_trace(data = df_text_mat1(), x = ~place_x.., y = ~place_y..,
                           type = 'scatter', mode = 'text', text = ~place_text,
                           line = NULL)
  }
  return(p1)
})

#Material 2 graph----
output$p2 <- renderPlotly({
  p2 <- plot_ly(df2_mat2(), x=~week_num.., y=~stock2.., mode = 'lines',type = 'scatter',
              line = list(color = 'grey', width = 2),
              hovertemplate = paste(paste0('<extra></extra>Stock: %{y}\nWeek: %{x}')))
  
  p2 <- make_graph(p2)
  
  if (length(df_text_mat2()) > 0 ) {
    p2 <- p2 %>% add_trace(data = df_text_mat2(), x = ~receive_x.., y = ~receive_y..,
                           type = 'scatter', mode = 'text', text = ~receive_text,
                           line = NULL)
    #add text labels on 'place' vertical lines
    p2 <- p2 %>% add_trace(data = df_text_mat2(), x = ~place_x.., y = ~place_y..,
                           type = 'scatter', mode = 'text', text = ~place_text,
                           line = NULL)
  }
  
  return(p2)
})
}