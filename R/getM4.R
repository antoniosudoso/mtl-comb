library(M4comp2018)
library(rlist)
library(forecast)

#Retrieve M4 data
get_M4_data <- function(){
  data(M4)
}

#Get M4 list length
# @param dataset dataset (list format) of interest
get_dataset_len <- function(dataset){
  return(length(dataset))
}

#Print M4 summary info
# @param dataset (list format) of interest
print_m4 <- function(dataset){
  allperiod <- factor(sapply(dataset, "[[", "period"), levels = c("Yearly", "Quarterly", "Monthly", "Weekly", "Daily", "Hourly"))
  alltypes  <- factor(sapply(dataset, "[[", "type")  , levels = c("Demographic", "Finance", "Industry", "Macro", "Micro", "Other"))
  tab <- cbind(tab <- table(allperiod, alltypes, dnn = c("Period", "Type")), Total = rowSums(tab))
  tab <- rbind(tab, Total = colSums(tab))
  print(tab)
}

#Print single time series info
# @param dataset dataset (list format) of interest
# @param timeSeriesIndex time series index to get info about 
print_timeSeries <- function(dataset, timeSeriesIndex){
  cat(paste("ID    : ", dataset[[timeSeriesIndex]]$st, "\n"))
  cat(paste("Type  : ", dataset[[timeSeriesIndex]]$type, "\n"))
  cat(paste("Period: ", dataset[[timeSeriesIndex]]$period, "\n"))
  cat(paste("Historical data length: ", dataset[[timeSeriesIndex]]$n, "\n"))
  cat(paste("Future data length: ", dataset[[timeSeriesIndex]]$h, "\n\n"))
  
  cat("HISTORICAL DATA:\n")
  print(dataset[[timeSeriesIndex]]$x)
  cat("\nFUTURE DATA:\n")
  print(dataset[[timeSeriesIndex]]$xx)
}

#Retrieve time series in M4 from time series identifier ("st" field)
# @param dataset dataset (list format) of interest
# @param ts_st time series identifier
get_ts_from_identifier <- function(dataset, ts_st){
  timeSeries <- list.first(dataset, st == ts_st)
  return(timeSeries)
}

#Retrieve time series in M4 at given index
# @param dataset dataset (list format) of interest
# @param index index in the dataset
get_ts_from_index <- function(dataset, index){
  if (index <= 100000){
    timeSeries <- dataset[[index]]
  }else{
    print("Incorrect index\n")
    return(NULL)
  }
  return(timeSeries)
}

#Retrieve time series index in M4 from time series identifier ("st" field)
# @param dataset dataset (list format) of interest
# @param ts_st time series identifier
get_ts_index <- function(dataset, ts_st){
  index_vector <- list.findi(dataset, st == ts_st)
  if (length(index_vector) == 0){
    print("No such element")
  }else{
    return(index_vector[[1]])
  }
}


#Plot single time series observations. Historical data are shown in black, 
#future data in red
# @param timeSeries time series of interest
plot_timeSeries <- function(timeSeries){
  plot(ts(c(timeSeries$x, timeSeries$xx),
          start=start(timeSeries$x), frequency = frequency(timeSeries$x)),
       col="red", type="l", ylab="")
  lines(timeSeries$x, col="black")
}

#Get yearly time series
# @param dataset dataset (list format) of interest
get_M4_yearly <- function(dataset){
  return(Filter(function(l) l$period == "Yearly", dataset))
}

#Get quarterly time series
# @param dataset dataset (list format) of interest
get_M4_quarterly <- function(dataset){
  return(Filter(function(l) l$period == "Quarterly", dataset))
}

#Get monthly time series
# @param dataset dataset (list format) of interest
get_M4_monthly <- function(dataset){
  return(Filter(function(l) l$period == "Monthly", dataset))
}

#Get weekly time series
# @param dataset dataset (list format) of interest
get_M4_weekly <- function(dataset){
  return(Filter(function(l) l$period == "Weekly", dataset))
}

#Get daily time series
# @param dataset dataset (list format) of interest
get_M4_daily <- function(dataset){
  return(Filter(function(l) l$period == "Daily", dataset))
}

#Get hourly time series
# @param dataset dataset (list format) of interest
get_M4_hourly <- function(dataset){
  return(Filter(function(l) l$period == "Hourly", dataset))
}

#Get demographic type time series
# @param dataset dataset (list format) of interest
get_M4_demographic <- function(dataset){
  return(Filter(function(l) l$type == "Demographic", dataset))
}

#Get finance type time series
# @param dataset dataset (list format) of interest
get_M4_finance <- function(dataset){
  return(Filter(function(l) l$type == "Finance", dataset))
}

#Get industry type time series
# @param dataset dataset (list format) of interest
get_M4_industry <- function(dataset){
  return(Filter(function(l) l$type == "Industry", dataset))
}

#Get macro type time series
# @param dataset dataset (list format) of interest
get_M4_macro <- function(dataset){
  return(Filter(function(l) l$type == "Macro", dataset))
}

#Get micro type time series
# @param dataset dataset (list format) of interest
get_M4_micro <- function(dataset){
  return(Filter(function(l) l$type == "Micro", dataset))
}

#Get other type time series
# @param dataset dataset (list format) of interest
get_M4_other <- function(dataset){
  return(Filter(function(l) l$type == "Other", dataset))
}

#Get time series identifier ("st" field)
# @param timeSeries time series of interest
get_st <- function(timeSeries){
  return(timeSeries$st)
}

#Get number of historical observations ("n" field)
# @param timeSeries time series of interest
get_n <- function(timeSeries){
  return(timeSeries$n)
}

#Get number of future observations ("h" field)
# @param timeSeries time series of interest
get_h <- function(timeSeries){
  return(timeSeries$h)
}

#Get time series period ("period" field)
# @param timeSeries time series of interest
get_period <- function(timeSeries){
  return(timeSeries$period)
}

#Get time series type ("type" field)
# @param timeSeries time series of interest
get_type <- function(timeSeries){
  return(timeSeries$type)
}

#Get historical ts object ("x" field)
# @param timeSeries time series of interest
get_hist_ts <- function(timeSeries){
  return(timeSeries$x)
}

#Get future ts object ("xx" field)
# @param timeSeries time series of interest
get_future_ts <- function(timeSeries){
  return(timeSeries$xx)
}

#Get start of historical data
get_hist_start <- function(timeSeries){
  start<- attr(timeSeries$x, "tsp")[1]
  return(start)
}

#Get end of historical data
get_hist_end <- function(timeSeries){
  end <- attr(timeSeries$x, "tsp")[2]
  return(end)
}

#Get frequency of historical data
get_hist_frequency <- function(timeSeries){
  frequency <- attr(timeSeries$x, "tsp")[3]
  return(frequency)
}

#Get start of future data
get_future_start <- function(timeSeries){
  start<- attr(timeSeries$xx, "tsp")[1]
  return(start)
}

#Get end of future data
get_future_end <- function(timeSeries){
  end <- attr(timeSeries$xx, "tsp")[2]
  return(end)
}

#Get frequency of future data
get_future_frequency <- function(timeSeries){
  frequency <- attr(timeSeries$xx, "tsp")[3]
  return(frequency)
}

#Print, if any, time series index of time series with constant historical data
#For M4 data, there are not such time series. Return also the list with the
#identifiers of such time series
# @param dataset dataset (list format) of interest
find_constant_hist_data <- function(dataset){
  M4_constant_train <- sapply(dataset, function(temp){
    training <- temp$x
    if (is.constant(training)==TRUE){
      print(temp$st)
      temp$st
    }
  })
  rm_list <- Filter(function(l) !is.null(l), M4_constant_train)
  if(length(rm_list) == 0){
    cat("There isn't any constant training time series\n")
  }
  return(rm_list)
}

#Remove constant training time series
# @param dataset dataset (list format) of interest
rm_constant_ts <- function(dataset){
  rm_list <- find_constant_hist_data(dataset)
  return(Filter(function(l) !(l$st %in% rm_list), dataset))
}


#Print, if any, time series index of time series with partially constant historical data
#Return also the list with the identifiers of such time series
# @param dataset dataset (list format) of interest
# @param max_equal_obs maximum number of consecutive equal observations
find_partially_constant_hist_data <- function(dataset, max_equal_obs){
  M4_partially_constant_train <- sapply(dataset, function(temp){
    training <- temp$x
    curr <- NA
    prev <- NA
    counter <- 1
    is_partially_constant <- FALSE
    for (i in 1:length(training)){
      curr <- training[[i]]
      if (!is.na(prev) && curr == prev){
        counter <- counter + 1
        if (counter > max_equal_obs){
          print(temp$st)
          is_partially_constant <- TRUE
          break
        }
      }else{
        counter <- 1
      }
      prev <- curr
    }
    if (is_partially_constant == TRUE){
      temp$st
    }
  })
  #print(M4_partially_constant_train)
  #rm_list <- M4_constant_train[-which(sapply(M4_constant_train, is.null))]
  #print(rm_list)
  rm_list <- Filter(function(l) !is.null(l), M4_partially_constant_train)
  if(length(rm_list) == 0){
    cat("There isn't any partially constant training time series\n")
  }
  return(rm_list)
}

#Remove partially constant training time series
# @param dataset dataset (list format) of interest
# @param max_equal_obs maximum number of consecutive equal observations
rm_partially_constant_ts <- function(dataset, max_equal_obs){
  rm_list <- find_partially_constant_hist_data(dataset, max_equal_obs)
  return(Filter(function(l) !(l$st %in% rm_list), dataset))
}


#Print, if any, time series index of time series with less than min_length historical
#observations
#For M4 data, the minimum length of training is 13. Return also the list with the
#identifier of such time series
# @param dataset dataset (list format) of interest
# @param min_length minimum length of historical observations
find_short_hist_data <- function(dataset, min_length){
  M4_short_train <- sapply(dataset, function(temp){
    training <- temp$x
    if (length(training) < min_length){
      print(temp$st)
      temp$st
    }
  })
  rm_list <- Filter(function(l) !is.null(l), M4_short_train)
  if(length(rm_list) == 0){
    print(cat("There isn't any time series with less than ", min_length, "historical observations\n"))
  }
  return(rm_list)
}

#Remove time series with less than min_length historical observations
# @param dataset dataset (list format) of interest
# @param min_length minimum length of historical observations
rm_short_ts <- function(dataset, min_length){
  rm_list <- find_short_hist_data(dataset, min_length)
  return(Filter(function(l) !(l$st %in% rm_list), dataset))
}

#Split the dataset in training set and test set
# @param dataset dataset (list format) of interest
# @param num_test_ts number of time series in test set
# @param seed seed for sampling
split_training_test <- function(dataset, num_test_ts, seed){
  set.seed(seed)
  if(num_test_ts > length(dataset)){
    print("Too many time series requested as test set")
    return
  }
  indices <- sample(1:length(dataset), num_test_ts)
  M4train <- dataset[-indices]
  M4test <- dataset[indices]
  return(list(M4train, M4test))
}

# update frequency for all series in M4 dataset
cal_total_dataset_new_frequency <- function(dataset){
  yearly_M4 <- get_M4_yearly(M4)
  quarterly_M4 <- get_M4_quarterly(M4)
  monthly_M4 <- get_M4_monthly(M4)
  weekly_M4 <- get_M4_weekly(M4)
  daily_M4 <- get_M4_daily(M4)
  hourly_M4 <- get_M4_hourly(M4)
  yearly_new_freq <- change_frequency(yearly_M4, 1)
  quarterly_new_freq <- change_frequency(quarterly_M4, 4)
  new_dataset <- c(yearly_new_freq, quarterly_new_freq)
  monthly_new_freq <- change_frequency(monthly_M4, 12)
  new_dataset <- c(new_dataset, monthly_new_freq)
  weekly_new_freq <- change_frequency(weekly_M4, 4)
  new_dataset <- c(new_dataset, weekly_new_freq)
  daily_new_freq <- change_frequency(daily_M4, 7)
  new_dataset <- c(new_dataset, daily_new_freq)
  hourly_new_freq <- change_frequency(hourly_M4, 24)
  new_dataset <- c(new_dataset, hourly_new_freq)
  new_dataset
}