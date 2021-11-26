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

#vector for the x axis to help with the fill effects
x_axis <- rep(0,term_mo+1)

df <- data.frame(month,market_balance,cash,mortgage_expenditure,
                 mortgage_expenditure_wo_fee, x_axis)

#make df 'tidy' (ie arranged so all EUR values are in 1 column)
#note that I give it a new name, since I'll need original df in geom_ribbon
df_tidy <- df %>% pivot_longer(!month,names_to = 'balance_type',values_to = 'euro')

#sort the df
df_tidy <- df_tidy[order(df_tidy$balance_type,df_tidy$month),]

#define colors
darkerRed=rgba(255,0,0,.6)
faintRed <- rgba(255,0,0,.3)
faintOrange <- rgba(255,165,0,.3)
faintBlue <- rgba(0,0,255,.2)


ggplot(data = df_tidy, aes(x = month, y = euro, group = balance_type, 
                      linetype = balance_type))+
  geom_line()+
  #I'm choosing the linetypes and labels for legend below. the MANUAL function
  #is used because I'm manually setting the linetype
  scale_linetype_manual(values=c('solid','dotted','longdash','blank','blank'),
                        labels=c('Cash Baseline','Market Return',
                                 'Total Mortage Expenditure','',''))+
  geom_ribbon(data=df,aes(x=month,ymin=mortgage_expenditure_wo_fee, ymax=mortgage_expenditure,fill='red',alpha=0.6),
              inherit.aes=FALSE)+
  geom_ribbon(data=df,aes(x=month,ymin=cash, ymax=mortgage_expenditure_wo_fee,fill='tomato',alpha=0.3),
              inherit.aes=FALSE)+
  geom_ribbon(data=df,aes(x=month,ymin=mortgage_expenditure, ymax=market_balance, fill='orange',alpha=0.3),
              inherit.aes=FALSE)+
  geom_ribbon(data=df,aes(x=month,ymin=x_axis, ymax=cash,fill='blue',alpha=0.2),
              inherit.aes=FALSE)+
  scale_fill_manual(name='',guide='legend',
                    values=c('red'='red','tomato'='tomato', 'orange'='orange'),
                    labels=c('Prepayment Penalty','Interest','Market Return Above Mortgage'))+
  theme(legend.title=element_blank())+
  ylim(0,NA)
  
#now, trying to get the legend for the fill colors to account for the alpha
#I'm trying to save rgba colors so I can redefine them in the geom_ribbon()'s,
#but i cant figure out how to save an rgba format in r
