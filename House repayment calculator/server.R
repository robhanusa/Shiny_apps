library(tidyverse)
library(ggplot2)

term_yr <- 10
mortgage_rate_yr <- 0.015
start_balance <- 100000
market_rate_yr <- 0.1
early_pentalty <- 0.05

term_mo <- term_yr*12
mortgage_rate_mo <- (mortgage_rate_yr+1)^(1/12)-1
market_rate_mo <- (market_rate_yr+1)^(1/12)-1


#annuity formula below found on https://www.wallstreetmojo.com/mortgage-formula/
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
mortgage_cumulative <- seq(0,monthly_payment*term_mo,monthly_payment)

#remaining balance
outstanding_mortgage <- c(start_balance,rep(0,term_mo))
for(i in c(2:length(outstanding_mortgage))){
  outstanding_mortgage[i] <- outstanding_mortgage[i-1]*(1+mortgage_rate_mo)-monthly_payment
}

#cost of early repayment
early_repayment_cost <- rep(0,term_mo+1)
for(i in c(1:length(early_repayment_cost))){
  early_repayment_cost[i] <- outstanding_mortgage[i]*(1+early_pentalty)
}

df <- data.frame(month,market_balance)

ggplot(data = df, aes(x = month))+
  geom_line(aes(y = market_balance))+
  geom_line(aes(y = mortgage_cumulative))+
  geom_line(aes(y = outstanding_mortgage))+
  geom_line(aes(y = early_repayment_cost))

