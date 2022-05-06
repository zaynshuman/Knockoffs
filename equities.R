library("readr")
library("tidyverse")

data_2020 <- read_csv("H:\\Dataset\\dataset_2020.csv")
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

data_2020<-subset(data_2020, begdat < "2010-01-01")
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

tail(data_2020)

perma <- subset(data_2020, date == "2020-12-31")

# We have 2520 US Equities to choose from 


mkt_cap = perma$shrout * abs(perma$prc)
perma <- data.frame(perma,mkt_cap)
perma <- perma[rev(order(perma$mkt_cap)),]
perma = subset(perma, select = -c(1,3,4,5,6,7,8,9,10,11,12) )


perma <- perma %>% distinct(permno, .keep_all = TRUE)
perma <- perma[1:102,]
perma <-perma[-c(14,22),]

# Remove unnecessary cols from the data frame

data_2020 = subset(data_2020, select = -c(1,5,6,7,8,9,10,11,12) )
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

data <- rbind(data_2020,data_2019,data_2018,data_2017,data_2016,data_2015,data_2014,data_2013,data_2012, data_2011, data_2010)
data <- data[rev(order(data$date)),]

dates_list <- data %>% distinct(date, .keep_all = TRUE)
dates_list <- dates_list[,2]

friday_iter = as.Date("2010-01-08")
friday_vec = c(1:572)
friday_vec[1] = friday_iter
for (i in 1:572){
  friday_iter = friday_iter + 7
  friday_vec[i+1] = friday_iter
}

friday_vec <- as.Date(friday_vec, origin = "1970-01-01")
friday_vec <- as.Date(friday_vec, format = "%Y/%m/%d")
friday_vec



mat <- matrix(list(), nrow = length(friday_vec), ncol = 100)

# For each permno and for each friday, we want to search for the corresponding price
counter_j = 1
for (j in perma$permno){
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


