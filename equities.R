library("readr")
library("tidyverse")

#data_2020 <- read_csv("H:\\Dataset\\dataset_2020.csv")
#head(data_2020)
data_2019 <- read_csv("H:\\Dataset\\dataset_2019.csv")
data_2018 <- read_csv("H:\\Dataset\\dataset_2018.csv")
data_2017 <- read_csv("H:\\Dataset\\dataset_2017.csv")
data_2016 <- read_csv("H:\\Dataset\\dataset_2016.csv")
data_2015 <- read_csv("H:\\Dataset\\dataset_2015.csv")
data_2014 <- read_csv("H:\\Dataset\\dataset_2014.csv")
data_2013 <- read_csv("H:\\Dataset\\dataset_2013.csv")
data_2012 <- read_csv("H:\\Dataset\\dataset_2012.csv")
data_2011 <- read_csv("H:\\Dataset\\dataset_2011.csv")
data_2010 <- read_csv("H:\\Dataset\\dataset_2010.csv")

# Remove all rows with begdat less than 2010-01-01

#data_2020<-subset(data_2020, begdat < "2010-01-01")
data_2019<-subset(data_2019, begdat < "2010-01-01")
data_2018<-subset(data_2018, begdat < "2010-01-01")
data_2017<-subset(data_2017, begdat < "2010-01-01")
data_2016<-subset(data_2016, begdat < "2010-01-01")
data_2015<-subset(data_2015, begdat < "2010-01-01")
data_2014<-subset(data_2014, begdat < "2010-01-01")
data_2013<-subset(data_2013, begdat < "2010-01-01")
data_2012<-subset(data_2012, begdat < "2010-01-01")
data_2011<-subset(data_2011, begdat < "2010-01-01")
data_2010<-subset(data_2010, begdat < "2010-01-01")

# Find tickers for largest companies by Mkt cap on last day of training period 


large_eq <- subset(data_2019, date == "2019-12-31")

mkt_cap = large_eq$shrout * abs(large_eq$prc)
large_eq <- data.frame(large_eq,mkt_cap)
large_eq <- large_eq[rev(order(large_eq$mkt_cap)),]
large_eq = subset(large_eq, select = -c(1,3,4,5,6,7,8,9,10,11,12) )

large_eq <- large_eq %>% distinct(permno, .keep_all = TRUE)
large_eq <- large_eq[1:102,]
large_eq <-large_eq[-c(10,19),]
large_eq <- large_eq[1:100,]

# Remove unnecessary cols from the data frame

#data_2020 = subset(data_2020, select = -c(1,5,6,7,8,9,10,11,12) )
data_2019 = subset(data_2019, select = -c(1,5,6,7,8,9,10,11,12) )
data_2018 = subset(data_2018, select = -c(1,5,6,7,8,9,10,11,12) )
data_2017 = subset(data_2017, select = -c(1,5,6,7,8,9,10,11,12) )
data_2016 = subset(data_2016, select = -c(1,5,6,7,8,9,10,11,12) )
data_2015 = subset(data_2015, select = -c(1,5,6,7,8,9,10,11,12) )
data_2014 = subset(data_2014, select = -c(1,5,6,7,8,9,10,11,12) )
data_2013 = subset(data_2013, select = -c(1,5,6,7,8,9,10,11,12) )
data_2012 = subset(data_2012, select = -c(1,5,6,7,8,9,10,11,12) )
data_2011 = subset(data_2011, select = -c(1,5,6,7,8,9,10,11,12) )
data_2010 = subset(data_2010, select = -c(1,5,6,7,8,9,10,11,12) )

data <- rbind(data_2019,data_2018,data_2017,data_2016,data_2015,data_2014,data_2013,data_2012, data_2011, data_2010)
data <- data[rev(order(data$date)),]
head(data)

dates_list <- data %>% distinct(date, .keep_all = TRUE)
dates_list <- dates_list[,2]

friday_iter = as.Date("2010-01-08")
friday_vec = c(1:520)
friday_vec[1] = friday_iter
for (i in 1:520){
  friday_iter = friday_iter + 7
  friday_vec[i+1] = friday_iter
}

friday_vec <- as.Date(friday_vec, origin = "1970-01-01")
friday_vec <- as.Date(friday_vec, format = "%Y/%m/%d")
friday_vec

length(friday_vec)

mat <- matrix(list(), nrow = length(friday_vec), ncol = 100)


# For each PERMNO and for each Friday, we want to search for the corresponding price
counter_j = 1
for (j in large_eq$permno){
  counter_i = 1
  stock_dates = subset(data, permno == j)
  for (i in friday_vec){
    updated_date = date_near(stock_dates$date,as.Date(i, origin = "1970-01-01"),sidepref='l')
    val = data$prc[data$permno==j & data$date == as.Date(updated_date, origin = "1970-01-01")]
    mat[counter_i,counter_j] = val[1]
    counter_i = counter_i + 1
  }
  print(counter_j)
  counter_j = counter_j +1 
}

mat <- mat[c(1:521),]

for (j in 1:100){
  for (i in 1:length(friday_vec)){
    mat[i,j] <- abs(as.numeric(mat[i,j]))
  }
}

return_series_mat = matrix(list(), nrow = length(friday_vec), ncol = 100)
for (j in 1:length(large_eq$permno)){
  for (i in 1:(length(friday_vec)-1)){
    return_series_mat[i+1,j] = ( as.numeric(mat[i+1,j]) / as.numeric(mat[i,j]) ) - 1 
  }
}

return_series_mat <- return_series_mat[-1,]
return_series_mat
