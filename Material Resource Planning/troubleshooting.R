
cons1_per_prod <- data.frame(prod_1 = rep(12,52*1.5),
                             prod_2 = rep(c(12,9),26*1.5),
                             prod_3 = rep(28,52*1.5))

make_cons_in_days <- function(cons_per_prod) {
  #chart_length <- 7*(52*1.5) #year and a half
  cons_list <- list(NULL,NULL,NULL)
  cols <- colnames(cons_per_prod)
  for (prod in 1:length(cons_per_prod)) {
    daily_cons <- rep(0,7*nrow(cons_per_prod[prod]))
    for (week in 1:nrow(cons_per_prod[prod])){
      for (day in 1:7) {
        daily_cons[(week-1)*7 + day] <- cons_per_prod[week,cols[prod]]/7
        #print(paste0('week:',week,'  day:',day,'  prod:',prod,'  calc:',cons1_per_prod[week,cols[prod]]))
      }
    }
    cons_list[[prod]] <- as.vector(daily_cons)
  }
  names(cons_list) <- cols
  return(cons_list)
}

  
cons1_daily <- make_cons_in_days(cons1_per_prod)

prod_starts <- as.Date(c("2021-12-27", "2021-12-27", "2021-12-27"))

begin_date <- as.Date('2021-11-01')

cols <- names(cons1_daily) 
cons_tot <- rep(0,52*7*1.5)
for (i in 1:length(cons_tot)){ #need to turn i into date, to iterate from start date to length of cons_tot
  #for (i in begin_date:(begin_date+length(cons_tot))){
  for (j in 1:3) {
    if(TRUE && (begin_date + i) >= prod_starts[j]){
      cons_tot[i] <- cons_tot[i] + cons1_daily[[cols[j]]][i]
    }
  }
}

#cons1_daily[['prod_1']][1]

week_num <- seq.Date(from = begin_date, to = begin_date +length(cons_tot),by = 1)
