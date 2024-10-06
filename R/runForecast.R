source("baseForecasters.R")
library(forecast)

#Processes forecast methods on a series given a series component and list of forecast methods
# @param seriesdata series component
# @param methods_list list of base forecasters

process_forecast_methods <- function(seriesdata, methods_list) {
  print(seriesdata$st)
  #process each method in methods_list to produce the forecasts and the errors
  lapply(methods_list, function (mentry) {
    method_name <- mentry
    method_fun <- get(mentry)
    forecasts <- tryCatch(method_fun(x=seriesdata$x, h=seriesdata$h),
                          error=function(error) {
                            print(error)
                            print(paste("Error processing series: ", seriesdata$st))
                            print(paste("The forecast method that produced the error is:",
                                        method_name))
                            print("Returning snaive forecasts instead")
                            snaive_forec(seriesdata$x, seriesdata$h)
                          })
    list(forecasts=forecasts, method_name=method_name)
  })
}



#Generate Forecasts for a Time Series Dataset

# @param dataset must be a list with each element having the following format:
#   x: a time series object ts with the historical data
#   h: the number of required forecasts
#
# @param methods a list of strings with the names of the functions that generate the
# forecasts. The functions must exist and take as parameters (x, h), with
# x being the ts object with the input series and h the number of required
# forecasts (after the last observation of x). 
#
# @param only_point_forec boolean that indicates to return only the point forecasts
#(if TRUE) or the entire model objects (if FALSE)
#
# @param n.cores The number of cores to be used. n.cores > 1 means parallel processing.

# Returns the original dataset where each element contains also the base forecasters' point forecasts (if only_point_forec is TRUE) 
# or the entire model objects (if only_point_forec is FALSE)

calc_forecasts <- function(log_file, dataset, methods, n.cores=1, only_point_forec=TRUE) {
  list_process_fun <- lapply
  cl = -1
  
  if (n.cores > 1) {
    cl <- parallel::makeCluster(n.cores, outfile=log_file)
    .env <- as.environment(as.list(environment(process_forecast_methods), all.names = TRUE))
    lapply(methods, function(method) assign(method, get(method), .env))
    parallel::clusterExport(cl, varlist=ls(envir=.env), envir = .env)
    list_process_fun <- function(my_list, ...) {
      parallel::parLapplyLB(cl, my_list, ...)
    }
  }
  
  ret_list <- list_process_fun(dataset, function (seriesdata) {
    results <- process_forecast_methods(seriesdata, methods)
    if (only_point_forec) {
      ff <- t(sapply(results, function (resentry) resentry$forecasts))
      method_names <- sapply(results, function (resentry) resentry$method_name)
      row.names(ff) <- method_names
      ff
    } else {
      results
    }
  })
  
  if (n.cores > 1) {
    parallel::stopCluster(cl)
  }
  
  ret_list
}