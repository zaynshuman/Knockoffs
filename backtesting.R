#index

backtest_price <- price_hist[522:573]
 
#mat is our matrix of asset prices
#equity
mat_2020 <- mat[522:573,]
prc_2020 = c(1:52)

for (k in 1:52){
  counter = 1
  for (i in selected3){
    prc_2020[k] = prc_2020[k] + weight[counter] * as.numeric(mat_2020[k,i])  
    counter = counter + 1 
    }
}

plot(1:52, prc_2020, type = 'l', xlab = 'Weeks in 2020', ylab = 'Tracking Portfolio Price')
plot(1:52, backtest_price, type = 'l',xlab = 'Weeks in 2020', ylab =  'DIJA Price' )




