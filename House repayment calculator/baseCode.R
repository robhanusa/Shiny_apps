library(tidyverse)
library(ggplot2)
library(plyr)
library(glue)

#start balance in thousands
start_balance <- 100
term_yr <- 10
mortgage_rate_yr <- 0.015
market_rate_yr <- 0.1
early_pentalty <- 0.05
include_market_return <- TRUE
monthly_budget <- 1000

term_mo <- term_yr*12
mortgage_rate_mo <- (mortgage_rate_yr+1)^(1/12)-1
market_rate_mo <- (market_rate_yr+1)^(1/12)-1

#"annuity formula" below found on https://www.wallstreetmojo.com/mortgage-formula/
monthly_payment <- start_balance*mortgage_rate_mo*(1+mortgage_rate_mo)^(term_mo)/
  ((1+mortgage_rate_mo)^(term_mo)-1)

#x axis vector
month <- c(0:term_mo)

#balance if all the money stayed in the market
market_balance <- c(start_balance,rep(0,term_mo))
for (i in c(2:length(market_balance))){
  market_balance[i] <- market_balance[i-1]*(1+market_rate_mo)
}

#cumulative sum paid for mortgage
mortgage_paid <- seq(0,monthly_payment*term_mo,monthly_payment)

#remaining balance
outstanding_mortgage <- c(start_balance,rep(0,term_mo))
for(i in c(2:length(outstanding_mortgage))){
  outstanding_mortgage[i] <- outstanding_mortgage[i-1]*(1+mortgage_rate_mo)-monthly_payment
}

#expenditure if paid in full at each time period. Calculated by
#(mortgage already paid)+(outstanding balance)+(early payment penalty on outstanding balance)
mortgage_expenditure <- rep(0,term_mo+1)
for (i in c(1:length(mortgage_expenditure))){
  mortgage_expenditure[i] <- mortgage_paid[i]+outstanding_mortgage[i]*(1+early_pentalty)
}

#to visualize the size of the fee, I'm making the line below of mortgage
#expenditure minus the fee.
mortgage_expenditure_wo_fee <- rep(0,term_mo+1)
for (i in c(1:length(mortgage_expenditure_wo_fee))){
  mortgage_expenditure_wo_fee[i] <- mortgage_paid[i]+outstanding_mortgage[i]
}

#to compare to a baseline of cash
cash <- rep(start_balance,term_mo+1)

#vector for the x axis to help with the fill effects
x_axis <- rep(0,term_mo+1)

df <- data.frame(month,market_balance,cash,mortgage_expenditure,
                 mortgage_expenditure_wo_fee, x_axis)

#make df 'tidy' (ie arranged so all EUR values are in 1 column)
#note that I give it a new name, since I'll need original df in geom_ribbon
df_tidy <- df %>% pivot_longer(!month,names_to = 'balance_type',
                               values_to = 'euro')

#sort the df
df_tidy <- df_tidy[order(df_tidy$balance_type,df_tidy$month),]

#define colors
darkerRed <- rgb(255,0,0,alpha = .7*255,maxColorValue = 255)
faintRed <- rgb(255,0,0,alpha = .3*255,maxColorValue = 255)
faintOrange <- rgb(255,165,0,alpha = .3*255,maxColorValue = 255)
faintBlue <- rgb(0,0,255,alpha = .2*255,maxColorValue = 255)

#now allow user to toggle include_market_return

p_market <- ggplot(data = df_tidy, aes(x = month, y = euro, group = balance_type, 
                           linetype = balance_type))

p_noMarket <- ggplot(data = filter(df_tidy,df_tidy$balance_type != 'market_balance'), 
            aes(x = month, y = euro, group = balance_type,
                linetype = balance_type))
p_line <- geom_line(size = 0.8)
#I'm choosing the linetypes and labels for legend below. the MANUAL function
#is used because I'm manually setting the linetype
p_linetype <- scale_linetype_manual(values = c('solid','dashed','blank','blank'),
                        labels = c('Cash Baseline',
                                   'Total Mortage Expenditure','',''))
p_penalty_ribbon <- geom_ribbon(data = df,aes(x = month,ymin = mortgage_expenditure_wo_fee, 
                            ymax = mortgage_expenditure,fill = 'darkerRed'),
              inherit.aes = FALSE)
#note that I need to set show.legend = FALSE for all ribbons except 1, otherwise
#they overlap and the colors look darker
p_interest_ribbon <- geom_ribbon(data = df,aes(x = month,ymin = cash, 
                            ymax = mortgage_expenditure_wo_fee,fill = 'faintRed'),
              inherit.aes = FALSE, show.legend = FALSE)
p_base_ribbon <- geom_ribbon(data = df,aes(x = month,ymin = x_axis, 
                            ymax = cash,fill = 'faintBlue'),
              inherit.aes = FALSE,show.legend = FALSE)
p_fill_legend <- scale_fill_manual(name = '',guide = 'legend',
                    values = c(darkerRed = darkerRed, faintRed = faintRed,
                               faintBlue = faintBlue),
                    labels = c('Prepayment Penalty','Interest','Base'))
#remove title, increase size, remove gray background on line types
p_theme <- theme(legend.title = element_blank(),legend.key.size = unit(1.5,"lines"), 
        panel.border = element_blank(),
        panel.grid = element_blank())
p_xScale <- scale_x_continuous(name = 'Month',limits = c(0,term_mo), expand = c(0,0))

p_yScale <- scale_y_continuous(name = 'Euro in thousands',
                     limits = c(0,round_any(max(mortgage_expenditure),50,f = ceiling)),
                     expand = c(0,0))

#below are adjusted plot properties to include market balance comparison
p_linetype_market <- scale_linetype_manual(values = c('solid','dotted','dashed',
                                                      'blank','blank'),
                      labels = c('Cash Baseline','Market Return',
                                 'Total Mortage Expenditure','',''))

p_market_ribbon <- geom_ribbon(data = df,aes(x = month,ymin = mortgage_expenditure,
                                             ymax=market_balance, 
                                             fill='faintOrange'),
                               inherit.aes = FALSE, show.legend = FALSE)

p_fill_legend_market <- scale_fill_manual(name = '',guide = 'legend',
                                   values = c(darkerRed = darkerRed, 
                                              faintRed = faintRed,
                                              faintOrange = faintOrange, 
                                              faintBlue = faintBlue),
                                   labels = c('Prepayment Penalty','Interest',
                                              'Market Return Above Mortgage','Base'))

p_yScale_market <- scale_y_continuous(name = 'Euro in thousands',
                                        limits = c(0,round_any(max(market_balance),
                                                               50,f = ceiling)),
                                        expand = c(0,0))
if(include_market_return){
  p <- p_market + p_line + p_linetype_market + p_penalty_ribbon + p_interest_ribbon + 
    p_base_ribbon + p_market_ribbon + p_fill_legend_market + p_theme + p_xScale + 
    p_yScale_market
} else {
  p <- p_noMarket + p_line + p_linetype + p_penalty_ribbon + p_interest_ribbon + 
    p_base_ribbon + p_fill_legend + p_theme + p_xScale + p_yScale
}


#now want to create graph for monthly payment
monthly_payment_real <- round(monthly_payment*1000)

p_proto_bar <- geom_bar(position = 'stack', stat = 'identity', color = 'black')
p_proto_title <- ggtitle(glue('Monthly Payment:\nEUR {monthly_payment_real}'))
p_proto_theme <- theme(legend.position = 'none', plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(), axis.ticks = element_blank(),
        axis.text.y = element_text(size = 15),
        panel.background = element_blank(),panel.grid = element_blank())
p_proto_yScale <- scale_y_continuous(breaks = c(monthly_budget))
p_proto_xScale <- scale_x_discrete(expand = c(0,0))

if(monthly_budget > monthly_payment_real){
  payments <- c(monthly_budget - monthly_payment_real,monthly_payment_real)
  cat <- factor(c('a','b'), levels = c('a','b'))
  df_bar <- data.frame(payments, cat)
  p_budget <- ggplot(df_bar,aes(x = '',y = payments, fill = cat))
  p2 <- p_budget + p_proto_bar + p_proto_title + p_proto_theme + p_proto_yScale +
    p_proto_xScale + scale_fill_manual(values = c('white','green'))
} else {
  payments <- c(monthly_budget,monthly_payment_real - monthly_budget)
  cat <- factor(c('a','b'), levels = c('b','a'))
  df_bar <- data.frame(payments,cat)
  p_budget <- ggplot(df_bar,aes(x = '',y = payments, fill = cat))
  p2 <- p_budget + p_proto_bar + p_proto_title + p_proto_theme + p_proto_yScale +
    p_proto_xScale + scale_fill_manual(values = c('red','green'))
}
p2

