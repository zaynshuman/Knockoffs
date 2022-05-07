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

selected = c(1,3,4,12,14,21,24,26,30,33,34,35,42,47,53,57,58,60,62,63,67,69,71,73,74,76,94,95,97,100)
tic = c(1:30)
counter = 1
for (i in selected){
  tic[counter] = perma$tic[i]
  counter = counter + 1
}
tic
