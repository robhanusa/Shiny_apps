
cons1_per_prod <- data.frame(prod_1 = rep(12,52*1.5),
                             prod_2 = rep(c(12,9),26*1.5),
                             prod_3 = rep(28,52*1.5))

# make_cons_in_days <- function(cons_per_prod) {
  chart_length <- 7*(52*1.5) #year and a half
  cols <- colnames(cons1_per_prod)
  for (prod in 1:length(cons1_per_prod)) {
    daily_cons <- rep(0,7*nrow(cons1_per_prod[prod]))
    for (week in 0:nrow(cons1_per_prod[prod])-2){
      for (day in 1:7) {
        daily_cons[week*7 + day] <- cons1_per_prod[week,cols[prod]]/7
      }
    } 
  }
# }

  print(nrow(cons1_per_prod[prod]))
  
cons1_daily <- make_cons_in_days(cons1_per_prod)

