library("readxl")
library("lubridate")

date_near = function(dates, target, thresh = Inf, onlypre = F,
                     sidepref, quiet = T){
  uneven = 0
  if(length(thresh) > 2){
    warning("Threshold should be either length 1 or 2")
    return(NA)
  }
  if(length(thresh) == 2) {
    uneven = 1
  }
  
  ##Basic options
  target = unique(target)
  if(is.character(target)){target = as.Date(target)}
  if(length(target) != 1){
    warning("Target argument should only contain one Date")
    return(NA)
  }
  
  ## Remove points after if only before points desired
  if(onlypre == T){
    dates = dates[dates < target]
  }
  dates = unique(sort(dates))
  if(uneven){
    diff = target - dates
    dates = dates[dates > target - thresh[1] & dates < target + thresh[2]]
  } else{
    dates = dates[abs(dates-target)<thresh]
  }
  
  delts = as.numeric(abs(dates-target))
  if(length(delts) == 0){
    if(!quiet){
      warning("No Match found within threshold")
    }
    return(NA)
  }
  ind = delts == min(delts)
  date  = dates[ind]
  
  if(length(date) == 2){
    if(missing(sidepref)){
      date = date[sample(1:2,1)]
    }else if(sidepref == "l"){
      date = date[1]
    }else{
      date = date[2]
    }
  }
  return(date)
}

SP <- read_excel("H:\\Dataset\\SP500.xlsx")
head(SP)
SP$datadate <- lubridate::ymd(SP$datadate)
SP <- dplyr::arrange(SP, datadate)

friday_iter = as.Date(SP$datadate[6566])

friday_vec = c(1:630)
friday_vec[1] = friday_iter
for (i in 1:629){
  friday_iter = friday_iter + 7
  friday_vec[i+1] = friday_iter
}

friday_vec <- as.Date(friday_vec, origin = "1970-01-01")
friday_vec <- as.Date(friday_vec, format = "%Y/%m/%d")
friday_vec

# Check that all dates are Friday: TRUE 
day <- weekdays(friday_vec)
day

friday_vec %in% SP$datadate


updated_dates = c(1:630)
for (i in 1:630){
  updated_dates[i] <- date_near(SP$datadate,friday_vec[i],sidepref='l')
}

updated_dates <- as.Date(updated_dates, origin = "1970-01-01")
updated_dates <- as.Date(updated_dates, format = "%Y/%m/%d")
updated_dates

day_updated <- weekdays(updated_dates)
day_updated

updated_dates %in% friday_vec == friday_vec %in% SP$datadate

price_hist = c(1:630)
for (i in 1:630){
  price_hist[i] <- SP$prccd[SP$datadate==updated_dates[i]]
}
return_series = c(1:630)
for (i in 1:629){
  return_series[i+1] <- (price_hist[(i+1)]/price_hist[i])-1
}

return_series <- return_series[-1]
return_series



# Return series 1 corresponds to returns on 15 Jan and ends on 24 Dec 2020

return_series_resp <- return_series[1:520]
return_series_resp
length(return_series_resp)

plot(1:520,return_series_resp,type = 'l',xlab='Week',ylab='S&P 500 Weekly Return')
abline(h=mean(return_series_resp), col = 'red')

mean(return_series_resp)
