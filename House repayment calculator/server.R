library(tidyverse)
library(ggplot2)
library(plyr)
library(glue)
library(shiny)
library(plotly)

# Define colors
faint_orange <- 'rgba(255,165,200,.3)'
darker_red <- 'rgba(255,0,0,.7)'
faint_red <- 'rgba(255,0,0,.3)'
faint_blue <- 'rgba(0,0,255,.2)'

server <- function(input,output,session) {
  
  output$p <- renderPlotly({
    
    # Convert percents to decimals
    mortgage_rate_yr_percent <- input$mortgage_rate_yr / 100
    market_rate_yr_percent <- input$market_rate_yr / 100
    early_penalty_percent <- input$early_penalty / 100
    
    term_mo <- input$term_yr * 12
    mortgage_rate_mo <- (mortgage_rate_yr_percent + 1) ^ (1 / 12) - 1
    market_rate_mo <- (market_rate_yr_percent + 1) ^ (1 / 12) - 1
    
    # "Annuity formula" below found on https://www.wallstreetmojo.com/mortgage-formula/
    monthly_payment <- input$start_balance * mortgage_rate_mo * (1 + mortgage_rate_mo) ^ term_mo /
      ((1 + mortgage_rate_mo) ^ term_mo - 1)
    
    # x-axis vector
    month <- 0:term_mo
    
    # Balance if all the money stayed in the market
    market_balance <- numeric(term_mo + 1) 
    market_balance[1] <- input$start_balance
    for (i in 2:length(market_balance)) {
      market_balance[i] <- market_balance[i - 1] * (1 + market_rate_mo)
    }
    
    # Cumulative sum paid for mortgage
    mortgage_paid <- seq(0, monthly_payment * term_mo, monthly_payment)
    
    # Remaining balance
    outstanding_mortgage <- numeric(term_mo + 1)
    outstanding_mortgage[1] <- input$start_balance
    for(i in 2:length(outstanding_mortgage)) {
      outstanding_mortgage[i] <- outstanding_mortgage[i - 1] * (1 + mortgage_rate_mo) - monthly_payment
    }
    
    # Expenditure if paid in full at each time period. Calculated by:
    # (mortgage already paid)+(outstanding balance)+(early payment penalty on outstanding balance)
    mortgage_expenditure <- numeric(term_mo + 1)
    for (i in seq_along(mortgage_expenditure)) {
      mortgage_expenditure[i] <- mortgage_paid[i] + outstanding_mortgage[i] * (1 + early_penalty_percent)
    }
    
    # To visualize the size of the fee, I'm making the line below of mortgage
    # expenditure minus the fee.
    mortgage_expenditure_wo_fee <- numeric(term_mo + 1)
    for (i in seq_along(mortgage_expenditure_wo_fee)) {
      mortgage_expenditure_wo_fee[i] <- mortgage_paid[i] + outstanding_mortgage[i]
    }
    
    # Compare to a baseline of cash
    cash <- rep(input$start_balance, term_mo + 1)
    
    # Vector for the x axis to help with the fill effects
    x_axis <- numeric(term_mo + 1)
    
    df <- data.frame(month, 
                     market_balance, 
                     cash, 
                     mortgage_expenditure,
                     mortgage_expenditure_wo_fee, 
                     x_axis
    )
    
    # Line graph with money growth over time
    p <- plot_ly(df, x = ~month, y = ~cash, name = 'Cash baseline', mode = 'lines',
                 type = 'scatter', line = list(color = 'black', dash = 'solid')
    )
    
    p <- p %>% add_trace(y = ~mortgage_expenditure, 
                         name = 'Mortgage cost',
                         line = list(color = 'black', dash = 'dash')
    )
    
    if(input$include_market_return) {
      p <- p %>% add_trace(y = ~market_balance, 
                           name = 'Market',
                           line = list(color = 'black', dash = 'dot'))
    }
    
    p <- p %>% add_trace(y = ~mortgage_expenditure_wo_fee, 
                         name = ' ',
                         line = list(color = 'white', dash = 'solid', width = 1)
    )
    
    p <- p %>% add_trace(y = ~x_axis, 
                         name = ' ',
                         line = list(color = 'white', dash = 'solid', width = 1)
    )
    
    p <- p %>% add_ribbons(ymin = ~mortgage_expenditure_wo_fee, 
                           ymax = ~mortgage_expenditure,
                           line = list(color = darker_red, width = 0), 
                           fillcolor = darker_red,
                           name = 'Early payment fee'
    )
    
    p <- p %>% add_ribbons(ymin = ~cash, 
                           ymax = ~mortgage_expenditure_wo_fee,
                           line = list(color = faint_red, width = 0), 
                           fillcolor = faint_red,
                           name = 'Mortgage interest'
    )
    
    p <- p %>% add_ribbons(ymin = ~x_axis, 
                           ymax = ~cash,
                           line = list(color = faint_blue, width = 0), 
                           fillcolor = faint_blue,
                           name = 'Base'
    )
    
    p <- p %>% layout(xaxis = list(title = 'Month'), 
                      yaxis = list(title = 'Euro in thousands')
    )
    
    if(input$include_market_return) {
      p <- p %>% add_ribbons(ymin = ~mortgage_expenditure, 
                             ymax = ~market_balance,
                             line = list(color = faint_orange, width = 0), 
                             fillcolor = faint_orange, 
                             name = 'Market return'
      )
    }
    
    return(p)
  })
  
  # Create graph for monthly payment
  output$p2 <- renderPlotly({
    
    mortgage_rate_yr_percent <- input$mortgage_rate_yr / 100
    market_rate_yr_percent <- input$market_rate_yr / 100
    early_penalty_percent <- input$early_penalty / 100
    
    term_mo <- input$term_yr * 12
    mortgage_rate_mo <- (mortgage_rate_yr_percent + 1) ^ (1 / 12) - 1
    market_rate_mo <- (market_rate_yr_percent + 1) ^ (1 / 12) - 1
    
    # Annuity formula
    monthly_payment <- input$start_balance * mortgage_rate_mo * (1 + mortgage_rate_mo) ^ term_mo /
      ((1 + mortgage_rate_mo) ^ term_mo - 1)
      
    monthly_payment_real <- round(monthly_payment * 1000)
    
    # Create plot components
    p_proto_bar <- geom_bar(position = 'stack', stat = 'identity', color = 'black')
    p_proto_title <- ggtitle(glue('Monthly Payment:\nEUR {monthly_payment_real}'))
    p_proto_yScale <- scale_y_continuous(breaks = c(input$monthly_budget))
    p_proto_xScale <- scale_x_discrete(expand = c(0, 0))
    p_proto_theme <- theme(legend.position = 'none', 
                           plot.title = element_text(hjust = 0.5),
                           axis.title = element_blank(), 
                           axis.ticks = element_blank(),
                           axis.text.y = element_text(size = 15),
                           panel.background = element_blank(),
                           panel.grid = element_blank()
    )
    
    # Create bar graph that shows partly red when payment is higher than budget,
    # and white when payment is less. Payment within budget is always green
    if(input$monthly_budget > monthly_payment_real) {
      payments <- c(input$monthly_budget - monthly_payment_real, monthly_payment_real)
      cat <- factor(c('a', 'b'), levels = c('a', 'b'))
      df_bar <- data.frame(payments, cat)
      fill_colors <- c('white', 'green')
    } else {
      payments <- c(input$monthly_budget, monthly_payment_real - input$monthly_budget)
      cat <- factor(c('a', 'b'), levels = c('b', 'a'))
      df_bar <- data.frame(payments, cat)
      fill_colors <- c('red', 'green')
    }
    
    # Create plot
    p_budget <- ggplot(df_bar, aes(x = '', y = payments, fill = cat))
    p2 <- p_budget + 
      p_proto_bar + 
      p_proto_title + 
      p_proto_theme + 
      p_proto_yScale +
      p_proto_xScale + 
      scale_fill_manual(values = fill_colors)
    
    return(p2)
  })
}