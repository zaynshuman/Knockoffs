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

Russ = read_excel("H:\\Russel100.xlsx")

Russ$datadate <- lubridate::ymd(Russ$datadate)
Russ <- dplyr::arrange(Russ, datadate)

monday_iter = as.Date(Russ$datadate[7839])
monday_vec = c(1:629)
for (i in 1:629){
  monday_iter = monday_iter + 7
  monday_vec[i] = monday_iter
}

monday_vec <- as.Date(monday_vec, origin = "1970-01-01")
monday_vec <- as.Date(monday_vec, format = "%Y/%m/%d")
monday_vec

# Check that all dates are Friday: TRUE 
day <- weekdays(monday_vec)
day

monday_vec %in% Russ$datadate


updated_dates = c(1:629)
for (i in 1:629){
  updated_dates[i] <- date_near(Russ$datadate,monday_vec[i],sidepref='l')
}

updated_dates <- as.Date(updated_dates, origin = "1970-01-01")
updated_dates <- as.Date(updated_dates, format = "%Y/%m/%d")
updated_dates

day_updated <- weekdays(updated_dates)
day_updated

updated_dates %in% monday_vec == monday_vec %in% Russ$datadate

price_hist = c(1:629)
for (i in 1:629){
  price_hist[i] <- Russ$RussPrice[Russ$datadate==updated_dates[i]]
}
return_series = c(1:629)
for (i in 1:628){
  return_series[i+1] <- (price_hist[(i+1)]/price_hist[i])-1
}

return_series <- return_series[-1]
return_series

plot(1:628,return_series,type = 'l')



