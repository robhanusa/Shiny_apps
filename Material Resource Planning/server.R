library(tidyverse)
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)

# Colors----
faintRed <- 'rgba(255, 0, 0, .5)'
faintGreen <- 'rgba(0, 255, 0, .5)'

# Material consumption per product by week of material 1
cons1_per_prod <- data.frame(
  prod_1 = rep(12, 52 * 1.5),
  prod_2 = rep(c(12, 9), 26 * 1.5),
  prod_3 = rep(28, 52 * 1.5)
)

# Material consumption per product by week of material 2
cons2_per_prod <- data.frame(
  prod_1 = rep(0, 52 * 1.5),
  prod_2 = rep(c(4, 3), 26 * 1.5),
  prod_3 = rep(35, 52 * 1.5)
)


server <- function(input,output,session) {

  # Toggle products ----
  
  # Ensure "product 1 start" week box does not appear if 'include prod 1' is unchecked
  observeEvent(input$include_prod1, {
      toggle("prod1_startDate")
    }, 
    ignoreInit = TRUE
  )
  
  observeEvent(input$include_prod2, {
      toggle("prod2_startDate")
    },
    ignoreInit = TRUE
  )
  
  observeEvent(input$include_prod3, {
      toggle("prod3_startDate")
    },
    ignoreInit = TRUE
  )
  
  # Toggle order inclusion ----
  # Ensure 'material 1, order 1' does not appear when 'include Order 1' is unchecked
  observeEvent(input$include_order1, {
      toggle("mat1_1")
      toggle("mat2_1")
      toggle('order1_arrivalDate')
    },
    ignoreInit = TRUE
  )
  
  observeEvent(input$include_order2, {
      toggle("mat1_2")
      toggle("mat2_2")
      toggle('order2_arrivalDate')
    },
    ignoreInit = TRUE
  )
  
  observeEvent(input$include_order3, {
      toggle("mat1_3")
      toggle("mat2_3")
      toggle('order3_arrivalDate')
    },
    ignoreInit = TRUE
  )
  
  begin_date <- reactive(input$order1_arrivalDate - input$lead_time * 7 - 30)
    
  # Convert weekly forecast to daily consumption ----
  make_cons_in_days <- function(cons_per_prod) {
    cons_list <- list(NULL, NULL, NULL)
    cols <- colnames(cons_per_prod)
    for (prod in seq_along(cons_per_prod)) {
      daily_cons <- rep(0, 7 * nrow(cons_per_prod[prod]))
      for (week in 1:nrow(cons_per_prod[prod])) {
        for (day in 1:7) {
          daily_cons[(week - 1) * 7 + day] <- cons_per_prod[week, cols[prod]] / 7
        }
      }
      cons_list[[prod]] <- as.vector(daily_cons)
    }
    names(cons_list) <- cols
    
    return(cons_list)
  }
  
  cons1_daily <- make_cons_in_days(cons1_per_prod)
  cons2_daily <- make_cons_in_days(cons2_per_prod)

  # Orders list----
  order_1 <- reactive(
    c(
      'mat_1' = ifelse(input$include_order1, input$mat1_1, 0),
      'mat_2' = ifelse(input$include_order1, input$mat2_1, 0),
      'arrival_date' = input$order1_arrivalDate
    )
  )
  
  order_2 <- reactive(
    c(
      'mat_1' = ifelse(input$include_order2, input$mat1_2, 0),
      'mat_2' = ifelse(input$include_order2, input$mat2_2, 0),
      'arrival_date' = input$order2_arrivalDate
    )
  )
  
  order_3 <- reactive(
    c(
      'mat_1' = ifelse(input$include_order3, input$mat1_3, 0),
      'mat_2' = ifelse(input$include_order3, input$mat2_3, 0),
      'arrival_date' = input$order3_arrivalDate
    )
  )
  
  orders <- reactive(list(order_1(), order_2(), order_3()))
  
  lead_time <- reactive(input$lead_time)
  
  # Allow toggle of all products, and to choose start week
  include_prods <- reactive(
    c(
      input$include_prod1,
      input$include_prod2,
      input$include_prod3
    )
  )
  
  prod_starts <- reactive(
    c(
      input$prod1_startDate,
      input$prod2_startDate,
      input$prod3_startDate
    )
  )
  
  # Function to calculate weekly consumption, based on consumption per product
  calc_consumption <- function(cons_per_prod) {
    cols <- names(cons_per_prod) 
    cons_tot <- rep(0, 52 * 7 * 1.5)
    for (i in seq_along(cons_tot)) {
      for (j in seq_along(include_prods())) {
        if(include_prods()[j] && (begin_date() + i) >= as.Date(prod_starts()[j])) {
          cons_tot[i] <- cons_tot[i] + cons_per_prod[[cols[j]]][i]
        }
      }
    }
    
    return(reactive(cons_tot))
  }
  
  cons_tot1 <- reactive(calc_consumption(cons1_daily))
  cons_tot2 <- reactive(calc_consumption(cons2_daily))
  
  # Calculate daily stock of each material.
  # Start stock is a float, consumption is a reactive vector, and the material 
  # specifies whether mat_1 or mat_2 is chosen
  calc_stock <- function(start_stock, consumption, material) {
    stock <- c(start_stock - consumption()[1], rep(0, 52 * 7 * 1.5 - 1))
    for (i in 2:length(stock)) {
      stock[i] <- stock[i - 1] - consumption()[i]
      for (order in orders()) {
        if (as.Date(order['arrival_date'], origin = "1970-01-01") == begin_date() + i) {
          stock[i] <- stock[i] + order[material]
        }
      }
    }
    
    return(stock)
  }

  stock1 <- reactive(calc_stock(input$mat1_start, cons_tot1(), 'mat_1'))
  stock2 <- reactive(calc_stock(input$mat2_start, cons_tot2(), 'mat_2'))

  x_date <- reactive(
    seq.Date(
      from = begin_date(), 
      to = begin_date() + length(stock1()) - 1, 
      by = 1
    )
  )
  x_axis <- reactive(rep(0, length(stock1())))
  
  # Note: the column names of the reactive elements have '..' at the end
  # But the new columns I add (ex stock1_pos) don't have these dots
  # This is important when rendering the graph
  
  # Dataframe for material 1----
  df_mat1 <- reactive(data.frame(x_date(), stock1(), x_axis()))
  
  # Create rows where only positive or negative are present, to make the red/green ribbons
  df1_mat1 <- reactive(mutate(df_mat1(), stock_pos = ifelse(stock1() > 0, stock1(), 0)))
  df2_mat1 <- reactive(mutate(df1_mat1(), stock_neg = ifelse(stock1() < 0, stock1(), 0)))
  
  # Filter df by date
  df3_mat1 <- reactive(
    filter(
      df2_mat1(), 
      x_date.. >= as.Date(input$date_range[1]) & x_date.. <= as.Date(input$date_range[2])
    )
  )
  
  # Dataframe for material 2----
  df_mat2 <- reactive(data.frame(x_date(), stock2(), x_axis()))
  
  # Create rows where only positive or negative are present, to make the red/green ribbons
  df1_mat2 <- reactive(
    mutate(
      df_mat2(), 
      stock_pos = ifelse(stock2() > 0, stock2(), 0)
    )
  )
  
  df2_mat2 <- reactive(
    mutate(
      df1_mat2(), 
      stock_neg = ifelse(stock2() < 0, stock2(), 0)
    )
  )
  
  df3_mat2 <- reactive(
    filter(
      df2_mat2(), 
      x_date.. >= as.Date(input$date_range[1]) & x_date.. <= as.Date(input$date_range[2])
    )
  )
   
  # Make vertical lines----
  vline <- function(x = 0, color = "orange") {
    list(
      type = "line",
      y0 = 0,
      y1 = 0.9,
      yref = "paper",
      x0 = x,
      x1 = x,
      line = list(color = color, dash = "solid")
    )
  }
  
  include_orders <- reactive(
    list(
      input$include_order1,
      input$include_order2,
      input$include_order3
    )
  )
  
  # Make list of vertical lines for arrival data and lead time, along with their locations
  make_vline_list <- function(include_orders, order, lead_time) {
    vline_list <- list()

    for (i in seq_along(include_orders)) {
      if (include_orders[[i]]) {
        arrival_date <- as.Date(order[[i]]['arrival_date'])
        vline_list[[length(vline_list) + 1]] <- vline(arrival_date)
        vline_list[[length(vline_list) + 1]] <- vline(arrival_date - lead_time * 7, color = 'red')
      }
    }

    return(vline_list)
  }
  
  vline_list <- reactive(make_vline_list(include_orders(), orders(), lead_time()))
  
  # Make text for orders----
  make_labels <- function(n, lead_time, stock) {
    # Make sure text position updates when orders() is changed
    if(!is.null(orders())) {
      receive_text <- c(paste0('Receive\norder ', n))
      receive_x <- reactive(c(as.Date(orders()[[n]]['arrival_date'], origin = "1970-01-01")))
      receive_y <- reactive(c(max(stock) * 1.1))
      
      # Make sure graph reacts when lead_time is changed
      if(lead_time) {
        place_text <- c(paste0('Place\norder ', n))
        place_x <- reactive(c(as.Date(orders()[[n]]['arrival_date'], origin = "1970-01-01") - lead_time * 7))
        place_y <- reactive(c(max(stock) * 1.1))
      }
    }
    
    return(
      reactive(
        data.frame(
          receive_text, 
          receive_x(), 
          receive_y(),
          place_text, 
          place_x(), 
          place_y()
        )
      )
    )
  }
  
  make_df_text <- function(include_orders, lead_time, stock) {
    text_list <- list()
    for (i in 1:3) {
      if(include_orders()[[i]]) {
        if(length(text_list) == 0) {
          text_list <- data.frame(c(make_labels(i, lead_time, stock)()))
          } else {
            text_list <- rbind(text_list, data.frame(c(make_labels(i, lead_time, stock)())))
          }
      }
    }
    
    return(text_list)
  }
  
  df_text_mat1 <- reactive(make_df_text(include_orders(), lead_time(), stock1()))
  df_text_mat2 <- reactive(make_df_text(include_orders(), lead_time(), stock2()))
  
  # Format graphs----
  
  # Create graphs with consistent formatting
  make_graph <- function(p) {
    p <- p %>% add_ribbons(
      ymin = ~x_axis..,
      ymax = ~stock_pos,
      line = list(color = 'black', width = 0),
      fillcolor = faintGreen
    )
    
    p <- p %>% add_ribbons(
      ymin = ~stock_neg,
      ymax = ~x_axis..,
      line = list(color = 'black', width = 0),
      fillcolor = faintRed
    )
    
    # Add vertical lines and format layout
    p <- p %>% layout(
      showlegend = FALSE,
      yaxis = list(title = ''),
      xaxis = list(
        title = 'Date',
        range = c(as.Date(input$date_range[1]), as.Date(input$date_range[2]))
      ),
      shapes = vline_list()
    )

    return(p)
  }

# Material 1 graph----
output$p1 <- renderPlotly({
  p1 <- plot_ly(
    df3_mat1(), 
    x=~x_date.., 
    y=~stock1.., 
    mode = 'lines',
    type = 'scatter',
    line = list(color = 'grey', width = 2),
    hovertemplate = paste(paste0('<extra></extra>Stock: %{y}\nWeek: %{x}'))
  )

  p1 <- make_graph(p1)
  
  # Add text labels on 'receive' vertical lines
  if (length(df_text_mat1()) > 0 ) {
    p1 <- p1 %>% add_trace(
      data = df_text_mat1(), 
      x = ~receive_x.., 
      y = ~receive_y..,
      type = 'scatter', 
      mode = 'text', 
      text = ~receive_text,
      line = NULL
    )
    
    # Add text labels on 'place' vertical lines
    p1 <- p1 %>% add_trace(
      data = df_text_mat1(), 
      x = ~place_x.., 
      y = ~place_y..,
      type = 'scatter', 
      mode = 'text', 
      text = ~place_text,
      line = NULL
    )
  }
  
  return(p1)
})

# Material 2 graph----
output$p2 <- renderPlotly({
  p2 <- plot_ly(
    df3_mat2(), 
    x=~x_date.., 
    y=~stock2.., 
    mode = 'lines',
    type = 'scatter',
    line = list(color = 'grey', width = 2),
    hovertemplate = paste(paste0('<extra></extra>Stock: %{y}\nDate: %{x}'))
  )
  
  p2 <- make_graph(p2)
  
  if (length(df_text_mat2()) > 0 ) {
    p2 <- p2 %>% add_trace(
      data = df_text_mat2(), 
      x = ~receive_x.., 
      y = ~receive_y..,
      type = 'scatter', 
      mode = 'text', 
      text = ~receive_text,
      line = NULL
    )
    
    # Add text labels on 'place' for vertical lines
    p2 <- p2 %>% add_trace(
      data = df_text_mat2(), 
      x = ~place_x.., 
      y = ~place_y..,
      type = 'scatter', 
      mode = 'text', 
      text = ~place_text,
      line = NULL
    )
  }

  return(p2)
})
}