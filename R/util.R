
# Removes fields 'pt_ff', 'up_ff' and 'low_ff' from the M4 dataset
# @param dataset: dataset of interest
remove_ff_fields <- function(dataset){
  new_dataset <- lapply(dataset, function(timeSeries){
    lentry <- list()
    lentry$st <- timeSeries$st
    lentry$x <- timeSeries$x
    lentry$n <- timeSeries$n
    lentry$type <- timeSeries$type
    lentry$h <- timeSeries$h
    lentry$period <- timeSeries$period
    lentry$xx <- timeSeries$xx
    lentry
  })
  new_dataset
}

# Change time series frequency
# @param dataset: dataset of interest
# @param frq: new frequency value
change_frequency <- function(dataset, frq){
  new_dataset <- lapply(dataset, function(timeSeries){
    new_ts <- ts(as.numeric(timeSeries$x), frequency=frq)
    lentry <- list()
    lentry$st <- timeSeries$st
    lentry$x <- new_ts
    lentry$n <- timeSeries$n
    lentry$type <- timeSeries$type
    lentry$h <- timeSeries$h
    lentry$period <- timeSeries$period
    lentry$xx <- ts(as.numeric(timeSeries$xx), frequency=frq)
    lentry
  })
  new_dataset
}

# Create validation set from M4 original dataset
# @param dataset: dataset of interest (df_yearly, df_monthly, etc)
M4_validation <- function(dataset){
  new_dataset <- lapply(dataset, function(timeSeries){
    ts_train = as.numeric(timeSeries$x)
    if (timeSeries$n < timeSeries$h) {
      print(sprintf("Skipping time series %s: length %d < horizon %d", timeSeries$st, timeSeries$h))
      NA
    } else {
      new_ts_train <- ts_train[1:(length(ts_train)-timeSeries$h)]
      new_ts_test <- ts_train[(length(ts_train)-timeSeries$h+1):length(ts_train)]
      new_ts_train <- ts(new_ts_train, frequency=frequency(timeSeries$x))
      new_ts_test <- ts(new_ts_test, frequency=frequency(timeSeries$x))
      lentry <- list()
      lentry$st <- timeSeries$st
      lentry$x <- new_ts_train
      lentry$n <- length(new_ts_train)
      lentry$type <- timeSeries$type
      lentry$h <- timeSeries$h
      lentry$period <- timeSeries$period
      lentry$xx <- new_ts_test
      lentry
    }
  })
  new_dataset
}