library(tidyverse)
library(ggplot2)
library(plyr)
library(glue)
library(shiny)
library(plotly)

#define colors
faintOrange <- 'rgba(255,165,200,.3)'
darkerRed <- 'rgba(255,0,0,.7)'
faintRed <- 'rgba(255,0,0,.3)'
faintBlue <- 'rgba(0,0,255,.2)'

server <- function(input,output,session){

output$p <- renderPlotly({
#convert percents to decimals
mortgage_rate_yr_percent <- input$mortgage_rate_yr/100
market_rate_yr_percent <- input$market_rate_yr/100
early_penalty_percent <- input$early_penalty/100

term_mo <- input$term_yr*12
mortgage_rate_mo <- (mortgage_rate_yr_percent+1)^(1/12)-1
market_rate_mo <- (market_rate_yr_percent+1)^(1/12)-1

#"annuity formula" below found on https://www.wallstreetmojo.com/mortgage-formula/
monthly_payment <- input$start_balance*mortgage_rate_mo*(1+mortgage_rate_mo)^(term_mo)/
  ((1+mortgage_rate_mo)^(term_mo)-1)

#x axis vector
month <- c(0:term_mo)

#balance if all the money stayed in the market
market_balance <- c(input$start_balance,rep(0,term_mo))
for (i in c(2:length(market_balance))){
  market_balance[i] <- market_balance[i-1]*(1+market_rate_mo)
}

#cumulative sum paid for mortgage
mortgage_paid <- seq(0,monthly_payment*term_mo,monthly_payment)

#remaining balance
outstanding_mortgage <- c(input$start_balance,rep(0,term_mo))
for(i in c(2:length(outstanding_mortgage))){
  outstanding_mortgage[i] <- outstanding_mortgage[i-1]*(1+mortgage_rate_mo)-monthly_payment
}

#expenditure if paid in full at each time period. Calculated by
#(mortgage already paid)+(outstanding balance)+(early payment penalty on outstanding balance)
mortgage_expenditure <- rep(0,term_mo+1)
for (i in c(1:length(mortgage_expenditure))){
  mortgage_expenditure[i] <- mortgage_paid[i]+outstanding_mortgage[i]*(1+early_penalty_percent)
}

#to visualize the size of the fee, I'm making the line below of mortgage
#expenditure minus the fee.
mortgage_expenditure_wo_fee <- rep(0,term_mo+1)
for (i in c(1:length(mortgage_expenditure_wo_fee))){
  mortgage_expenditure_wo_fee[i] <- mortgage_paid[i]+outstanding_mortgage[i]
}

#to compare to a baseline of cash
cash <- rep(input$start_balance,term_mo+1)

#vector for the x axis to help with the fill effects
x_axis <- rep(0,term_mo+1)

df <- data.frame(month,market_balance,cash,mortgage_expenditure,
                 mortgage_expenditure_wo_fee, x_axis)

p <- plot_ly(df, x = ~month, y = ~cash, name = 'Cash baseline', mode = 'lines',
               type = 'scatter', line = list(color = 'black', dash = 'solid'))
p <- p %>% add_trace(y = ~mortgage_expenditure, name = 'Mortgage cost',
                         line = list(color = 'black', dash = 'dash'))
if(input$include_market_return){
  p <- p %>% add_trace(y = ~market_balance, name = 'Market',
                       line = list(color = 'black', dash = 'dot'))
  }
p <- p %>% add_trace(y = ~mortgage_expenditure_wo_fee, name = ' ',
                         line = list(color = 'white', dash = 'solid', width = 1))
p <- p %>% add_trace(y = ~x_axis, name = ' ',
                         line = list(color = 'white', dash = 'solid', width = 1))
p <- p %>% add_ribbons(ymin = ~mortgage_expenditure_wo_fee, 
                           ymax = ~mortgage_expenditure,
                           line = list(color = darkerRed, width = 0), 
                           fillcolor = darkerRed,
                           name = 'Early payment fee')
p <- p %>% add_ribbons(ymin = ~cash, 
                           ymax = ~mortgage_expenditure_wo_fee,
                           line = list(color = faintRed, width = 0), 
                           fillcolor = faintRed,
                           name = 'Mortgage interest')
p <- p %>% add_ribbons(ymin = ~x_axis, 
                           ymax = ~cash,
                           line = list(color = faintBlue, width = 0), 
                           fillcolor = faintBlue,
                           name = 'Base')
p <- p %>% layout(xaxis = list(title = 'Month'), 
                      yaxis = list(title = 'Euro in thousands'))

if(input$include_market_return){
  p <- p %>% add_ribbons(ymin = ~mortgage_expenditure, ymax = ~market_balance,
                         line = list(color = faintOrange, width = 0), 
                         fillcolor = faintOrange, name = 'Market return')
}
return(p)
})

#now want to create graph for monthly payment
output$p2 <- renderPlotly({
#repeating code to make sure the 2nd graph also changes when it's updated
  mortgage_rate_yr_percent <- input$mortgage_rate_yr/100
  market_rate_yr_percent <- input$market_rate_yr/100
  early_penalty_percent <- input$early_penalty/100
  
  term_mo <- input$term_yr*12
  mortgage_rate_mo <- (mortgage_rate_yr_percent+1)^(1/12)-1
  market_rate_mo <- (market_rate_yr_percent+1)^(1/12)-1
  
monthly_payment <- input$start_balance*mortgage_rate_mo*(1+mortgage_rate_mo)^(term_mo)/
    ((1+mortgage_rate_mo)^(term_mo)-1)
  
monthly_payment_real <- round(monthly_payment*1000)

p_proto_bar <- geom_bar(position = 'stack', stat = 'identity', color = 'black')
p_proto_title <- ggtitle(glue('Monthly Payment:\nEUR {monthly_payment_real}'))
p_proto_theme <- theme(legend.position = 'none', plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(), axis.ticks = element_blank(),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),panel.grid = element_blank())
p_proto_yScale <- scale_y_continuous(breaks = c(input$monthly_budget))
p_proto_xScale <- scale_x_discrete(expand = c(0,0))

if(input$monthly_budget > monthly_payment_real){
  payments <- c(input$monthly_budget - monthly_payment_real,monthly_payment_real)
  cat <- factor(c('a','b'), levels = c('a','b'))
  df_bar <- data.frame(payments, cat)
  p_budget <- ggplot(df_bar,aes(x = '',y = payments, fill = cat))
  p2 <- p_budget + p_proto_bar + p_proto_title + p_proto_theme + p_proto_yScale +
    p_proto_xScale + scale_fill_manual(values = c('white','green'))
} else {
  payments <- c(input$monthly_budget,monthly_payment_real - input$monthly_budget)
  cat <- factor(c('a','b'), levels = c('b','a'))
  df_bar <- data.frame(payments,cat)
  p_budget <- ggplot(df_bar,aes(x = '',y = payments, fill = cat))
  p2 <- p_budget + p_proto_bar + p_proto_title + p_proto_theme + p_proto_yScale +
    p_proto_xScale + scale_fill_manual(values = c('red','green'))
}
return(p2)
})
}