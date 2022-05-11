install.packages("knockoff")
library("knockoff")
install.packages('Rcpp')
library("Rcpp")

return_series_mat <- matrix(unlist(return_series_mat), ncol =100, nrow =572)

knockoff.filter(
  return_series_mat,
  return_series_resp,
  knockoffs = create.second_order,
  statistic = stat.glmnet_coefdiff,
  fdr = 0.05,
  offset = 1
)

typeof(return_series_resp)


# Find equities associated with knockoffs 
# Update selected rows with the variables found via knockoffs, the code below identifies corresponding tickers 
selected = c()
tic = c(1:30)
counter = 1
for (i in selected){
  tic[counter] = perma$tic[i]
  counter = counter + 1
}
tic
