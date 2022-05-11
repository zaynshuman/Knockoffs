# Russell 1000 example

selected1 = c(1,   3 , 12,  14 , 15,  21 , 24 , 25  ,28,  29 , 30  ,33  ,35 , 37,  40 , 41,  42,  50,  51 , 52,  57 , 58  ,60,  62 , 63 , 66,  67 , 69 , 70 , 71 , 72 , 73,  74 , 76 , 80 , 84,  87 , 88,  91 , 95 , 97, 98 ,100)
weight = c(1:length(selected1))
sum_cap = 0

for (i in selected1){
  sum_cap = sum_cap + large_eq$mkt_cap[i] 
}


counter = 1
for (i in selected1){
  weight[counter] = large_eq$mkt_cap[i] / sum_cap
  counter = counter + 1
  
}

sum(weight)
weight

signif(weight,4)

# SP500 example

selected2 = c(1 ,  2   ,3,   4  ,11 , 12  ,14 , 15  ,23 , 24,  25 , 26  ,28 , 29  ,30 , 35  ,39 , 41,  42,  47  ,50 , 52  ,57 , 58  ,59 , 60  ,62 , 63  ,66 , 67  ,68  ,69  ,72 , 74,  80 , 81,  84 , 87,  89  ,91  ,95,100)
weight = c(1:length(selected2))
sum_cap = 0

for (i in selected2){
  sum_cap = sum_cap + large_eq$mkt_cap[i] 
}


counter = 1
for (i in selected2){
  weight[counter] = large_eq$mkt_cap[i] / sum_cap
  counter = counter + 1
  
}

sum(weight)
weight

signif(weight,4)

# DJIA example

selected3 = c(2 , 5,  6 , 7 ,10 ,11 ,12 ,14 ,16 ,21 ,24 ,25, 33, 39 ,52 ,55, 58 ,59 ,60 ,62 ,63, 69)
weight = c(1:length(selected3))
sum_cap = 0

for (i in selected3){
  sum_cap = sum_cap + large_eq$mkt_cap[i] 
}


counter = 1
for (i in selected3){
  weight[counter] = large_eq$mkt_cap[i] / sum_cap
  counter = counter + 1
  
}

sum(weight)
weight

signif(weight,4)
