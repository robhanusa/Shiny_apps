library(tidyverse)
library(shiny)
library(plotly)
library(dplyr)

#later, I'll need to import the following 2 datasets



server <- function(input,output,session){
  
  output$p1 <- renderPlotly({
    
    #material consumption per product by week of material 1
    cons1_per_prod <- data.frame(prod_1 = rep(12,52),
                                 prod_2 = rep(c(12,9),26),
                                 prod_3 = rep(28,52))
    
    #material consumption per product by week of material 2
    cons2_per_prod <- data.frame(prod_1 = rep(0,52),
                                 prod_2 = rep(c(4,3),26),
                                 prod_3 = rep(35,52))
    
    faintRed <- 'rgba(255,0,0,.5)'
    faintGreen <- 'rgba(0,255,0,.5)'
    

#add order info. I could consider turning this into a dataframe in the future
order_1 <- c('mat_1' = ifelse(input$include_order1,input$mat1_1,0),
            'mat_2' = ifelse(input$include_order1,input$mat2_1,0),
            'week' = 10)
order_2 <- c('mat_1' = ifelse(input$include_order2,input$mat1_2,0),
             'mat_2' = ifelse(input$include_order2,input$mat2_2,0),
             'week' = 24 )
order_3 <- c('mat_1' = ifelse(input$include_order3,input$mat1_3,0),
             'mat_2' = ifelse(input$include_order3,input$mat2_3,0),
             'week' = 34)

lead_time <- input$lead_time

#allow toggle of all products, and to choose start week
include_prod1 <- input$include_prod1
prod1_start <- input$prod1_start
include_prod2 <- input$include_prod2
prod2_start <-input$prod2_start
include_prod3 <- input$include_prod3
prod3_start <- input$prod3_start

#function to calculate weekly consumption. I can probably do this better with
#lapply than using the for loop- Revisit
calc_consumption <- function(cons_per_prod){
  cons_tot <- rep(0,52)
  for (i in 1:length(cons_tot)){
    if(include_prod1 && i >= prod1_start){
      cons_tot[i] <- cons_per_prod[i,"prod_1"]
    }
    if(include_prod2 && i >= prod2_start){
      cons_tot[i] <- cons_tot[i] + cons_per_prod[i,"prod_2"]
    }
    if(include_prod3 && i >= prod3_start){
      cons_tot[i] <- cons_tot[i] + cons_per_prod[i,"prod_3"]
    }
  }
  return(cons_tot)
}

cons1_tot <- calc_consumption(cons1_per_prod)
cons2_tot <- calc_consumption(cons2_per_prod)

#calculate weekly stock of each material

calc_stock <- function(start_stock, consumption,material){
  stock <- c(start_stock-consumption[1],rep(0,51))
  for (i in 2:length(stock)){
    stock[i] <- stock[i-1]-consumption[i]
    for (order in list(order_1,order_2, order_3)){
      if (order['week'] == i){
        stock[i] <- stock[i] + order[material] #where material <- 'mat_1' for example
      }
    }
  }
   return(stock)
}

stock1 <- calc_stock(input$mat1_start,cons1_tot,'mat_1')
stock2 <- calc_stock(input$mat2_start,cons2_tot,'mat_2')

week <- seq(1,length(stock1))
x_axis <- rep(0,length(stock1))

#need to create order time and make graphs
df <- data.frame(week,stock1,stock2,x_axis)
#create rows where only positive or negative are present, to make the red/green ribbons
df <- mutate(df,stock1_pos = ifelse(stock1 > 0, stock1, 0))
df <- mutate(df,stock1_neg = ifelse(stock1 < 0, stock1, 0))

#verticle line function
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
vline_list <- list()
if(input$include_order1){
  vline_list <- append(vline_list,list(vline(order_1['week']),
                                 vline(order_1['week'] - lead_time, color = 'red')))
}
if(input$include_order2){
  vline_list <- append(vline_list,list(vline(order_2['week']),
                                 vline(order_2['week'] - lead_time, color = 'red')))
}
if(input$include_order3){
  vline_list <- append(vline_list,list(vline(order_3['week']),
                                 vline(order_3['week'] - lead_time, color = 'red')))
}

#create df's for text labels on vertical lines
receive_text <- c('Receive\norder 1', 'Receive\norder 2', 'Receive\norder 3')
receive_x <- c(order_1['week'] + 2, 
               order_2['week'] + 2, 
               order_3['week'] + 2)
receive_y <- c(max(stock1), max(stock1), max(stock1))

place_text <- c('Place\norder 1', 'Place\norder 2', 'Place\norder 3')
place_x <- c(order_1['week'] - lead_time + 2, 
             order_2['week'] - lead_time + 2, 
             order_3['week'] - lead_time + 2)
place_y <- c(max(stock1)-100, max(stock1)-100, max(stock1)-100)

df_text <- data.frame(receive_text, receive_x, receive_y,
                      place_text, place_x, place_y)

#graphic for material 1
p1 <- plot_ly(df, x=~week, y=~stock1, mode = 'lines',type = 'scatter',
              line = list(color = 'grey', width = 2))

p1 <- p1 %>% add_ribbons(ymin = ~x_axis, 
                         ymax = ~stock1_pos,
                         line = list(color = 'black', width = 0),
                         fillcolor = faintGreen)
p1 <- p1 %>% add_ribbons(ymin = ~stock1_neg, 
                         ymax = ~x_axis,
                         line = list(color = 'black', width = 0),
                         fillcolor = faintRed)
#add vertical lines and format layout
p1 <- p1 %>% layout(showlegend = FALSE,
                    title = 'Material 1 Stock',
                    yaxis = list(title = ''),
                    xaxis = list(title = 'Week'))
p1 <- p1 %>% layout(shapes = vline_list)
#add text labels on 'receive' vertical lines
p1 <- p1 %>% add_trace(data = df_text, x = ~receive_x, y = ~receive_y,
                       type = 'scatter', mode = 'text', text = ~receive_text,
                       line = NULL)
#add text labels on 'place' vertical lines
p1 <- p1 %>% add_trace(data = df_text, x = ~place_x, y = ~place_y,
                       type = 'scatter', mode = 'text', text = ~place_text,
                       line = NULL)
p1

return(p1)
})
}
#next, plan out ui and turn into shiny app