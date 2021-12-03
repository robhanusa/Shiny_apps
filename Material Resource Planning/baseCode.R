library(tidyverse)
library(shiny)
library(plotly)

#later, I'll need to import the following 2 datasets

#material consumption per product by week of material 1
cons1_per_prod <- data.frame(prod_1 = rep(12,52),
                             prod_2 = rep(c(12,9),26),
                             prod_3 = rep(28,52))

#material consumption per product by week of material 2
cons2_per_prod <- data.frame(prod_1 = rep(0,52),
                              prod_2 = rep(c(4,3),26),
                              prod_3 = rep(35,52))

# #material A,B required for product 1,2,3
# df_mat_req <- data.frame(mat_1 = c(2,3,4),
#                          mat_2 = c(6,0,1))
# rownames(df_mat_req) <- c('prod_1','prod_2','prod_3')

#allow toggle of all products, and to choose start week
include_prod1 <- TRUE
prod1_start <- 2
include_prod2 <- TRUE
prod2_start <- 3
include_prod3 <- TRUE
prod3_start <- 4

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

#now that I have weekly consumption, need to calculate weekly stock

#starting stock
mat1_start <- 200
mat2_start <- 100

#add order info. I could consider turning this into a dataframe in the future
order_1 <- c('mat_1' = 200,
            'mat_2' = 100,
            'week' = 2)
order_2 <- c('mat_1' = 400,
             'mat_2' = 300,
             'week' = 6)
order_3 <- c('mat_1' = 300,
             'mat_2' = 200,
             'week' = 10)

lead_time <- 4

#initiate vectors of stock of material 1 and 2
stock1 <- c(mat1_start,rep(0,51))
stock2 <- c(mat2_start,rep(0,51))

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

stock1 <- calc_stock(mat1_start,cons1_tot,'mat_1')
stock2 <- calc_stock(mat2_start,cons2_tot,'mat_2')

#need to create order time and make graphs
