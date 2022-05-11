install.packages("knockoff")
library("knockoff")
install.packages('Rcpp')
library("Rcpp")
install.packages("doParallel")

return_series_mat <- matrix(unlist(return_series_mat), ncol =100, nrow =520)

knockoff.filter(
  return_series_mat,
  return_series_resp,
  knockoffs = create.second_order,
  statistic = stat.glmnet_coefdiff,
  fdr = 0.05,
  offset = 1
)



# Find equities associated with knockoffs 

selected1 = c(1,   3 , 12,  14 , 15,  21 , 24 , 25  ,28,  29 , 30  ,33  ,35 , 37,  40 , 41,  42,  50,  51 , 52,  57 , 58  ,60,  62 , 63 , 66,  67 , 69 , 70 , 71 , 72 , 73,  74 , 76 , 80 , 84,  87 , 88,  91 , 95 , 97, 98 ,100)
tic = c(1:30)
counter = 1
for (i in selected1){
  tic[counter] = large_eq$tic[i]
  counter = counter + 1
}
noquote(tic)

selected2 = c(1 ,  2   ,3,   4  ,11 , 12  ,14 , 15  ,23 , 24,  25 , 26  ,28 , 29  ,30 , 35  ,39 , 41,  42,  47  ,50 , 52  ,57 , 58  ,59 , 60  ,62 , 63  ,66 , 67  ,68  ,69  ,72 , 74,  80 , 81,  84 , 87,  89  ,91  ,95,100)
tic2 = c(1:30)
counter = 1
for (i in selected2){
  tic2[counter] = large_eq$tic[i]
  counter = counter + 1
}
noquote(tic2)

selected3 = c(2 , 5,  6 , 7 ,10 ,11 ,12 ,14 ,16 ,21 ,24 ,25, 33, 39 ,52 ,55, 58 ,59 ,60 ,62 ,63, 69)
tic3 = c(1:30)
counter = 1
for (i in selected3){
  tic3[counter] = large_eq$tic[i]
  counter = counter + 1
}
noquote(tic3)
