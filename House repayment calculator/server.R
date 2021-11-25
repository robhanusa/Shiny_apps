library(tidyverse)
library(ggplot2)

#start balance in thousands
start_balance <- 100
term_yr <- 10
mortgage_rate_yr <- 0.015
market_rate_yr <- 0.1
early_pentalty <- 0.05

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

df <- data.frame(month,market_balance,cash,mortgage_expenditure,mortgage_expenditure_wo_fee)

ggplot(data = df, aes(x = month))+
  geom_line(aes(y = market_balance,linetype = 'dotted'))+
  geom_line(aes(y = cash,linetype = 'solid'))+
  geom_line(aes(y = mortgage_expenditure, linetype = 'longdash'))+
  geom_line(aes(y = mortgage_expenditure_wo_fee), linetype = 'blank')+
  ylim(0,NA)+
  theme(legend.position='top', legend.title=element_blank())

#try graphing it differently below. Make df 'tidy' ie the way things would be
#organized in JMP with one column for all EUR values and one column for headers?
#https://community.rstudio.com/t/adding-manual-legend-to-ggplot2/41651


