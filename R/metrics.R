library(forecast)
library(stringr)

evaluate_metrics <- function(df_series, df_forecast, freq) {
  if (length(df_series) != nrow(df_forecast)) {
    stop('Incompatible length')
  }
  n <- length(df_series) 
  #metrics <- c('sOWA', 'SMAPE', 'MASE', 'sOWA_PKG', 'SMAPE_PKG', 'MASE_PKG')
  metrics <- c('sOWA', 'SMAPE', 'MASE', 'OWA')
  result <- matrix(NA, nrow=n, ncol=length(metrics))
  colnames(result) <- metrics
  
  sum_smape <- c()
  sum_smape_naive <- c()
  sum_mase <- c()
  sum_mase_naive <- c()
  
  for (i in 1:n) {
    
    ts_train <- ts(as.numeric(df_series[[i]]$ts_train), frequency=freq)
    ts_test <- ts(as.numeric(df_series[[i]]$ts_test), frequency=freq)
    ts_forecast <- ts(as.numeric(df_forecast[i, ]), frequency=freq)
    
    naive_2_forec <- naive_2(ts_train, length(ts_test))
    
    # smape and smape_naive_2
    smape <- smape_cal(as.numeric(ts_test), as.numeric(ts_forecast))
    smape_naive_2 <- smape_cal(as.numeric(ts_test), as.numeric(naive_2_forec))
    # mase and mase_naive_2
    mase <- mase_cal(ts_train, ts_test, ts_forecast)
    mase_naive_2 <- mase_cal(ts_train, ts_test, naive_2_forec)
    # sOWA
    sOWA <- sOWA_cal(smape, smape_naive_2, mase, mase_naive_2)
    
    sum_smape <- c(sum_smape, smape)
    sum_smape_naive <- c(sum_smape_naive, smape_naive_2)
    sum_mase <- c(sum_mase, mase)
    sum_mase_naive <- c(sum_mase_naive, mase_naive_2)
    
    #smape_PKG <- Metrics::smape(as.numeric(ts_test), as.numeric(ts_forecast))
    #smape_naive_2_PKG <- Metrics::smape(as.numeric(ts_test), as.numeric(naive_2_forec))
    #mase_PKG <- Metrics::mase(as.numeric(ts_test), as.numeric(ts_forecast), freq)
    #mase_naive_2_PKG <- Metrics::mase(as.numeric(ts_test), as.numeric(naive_2_forec), freq)
    #sOWA_PKG <- sOWA_cal(smape_PKG, smape_naive_2_PKG, mase_PKG, mase_naive_2_PKG)
    
    # store metrics for each series
    result[i, 1] <- sOWA
    result[i, 2] <- smape
    result[i, 3] <- mase
    
    #result[i, 4] <- sOWA_PKG
    #result[i, 5] <- smape_PKG
    #result[i, 6] <- mase_PKG
    
  }
  
  OWA <- 0.5*(sum(sum_smape, na.rm = TRUE)/sum(sum_smape_naive, na.rm = TRUE))+0.5*(sum(sum_mase, na.rm = TRUE)/sum(sum_mase_naive, na.rm = TRUE))
  
  result[i, 4] <- OWA
  
  return (result)
}

# --------------------------------
# Evaluation metrics and benchmark
# --------------------------------

SeasonalityTest <- function(input, ppy){
  # Used to determine whether a time series is seasonal
  tcrit <- 1.645
  if (length(input)<3*ppy){
    test_seasonal <- FALSE
  }else{
    xacf <- acf(input, plot = FALSE)$acf[-1, 1, 1]
    clim <- tcrit/sqrt(length(input)) * sqrt(cumsum(c(1, 2 * xacf^2)))
    test_seasonal <- ( abs(xacf[ppy]) > clim[ppy] )
    
    if (is.na(test_seasonal)==TRUE){ test_seasonal <- FALSE }
  }
  return(test_seasonal)
}

naive_2 <- function(input, fh){
  # Estimate seasonally adjusted time series
  ppy <- frequency(input); ST <- F
  if (ppy>1){ ST <- SeasonalityTest(input,ppy) }
  if (ST==T){
    Dec <- decompose(input,type="multiplicative")
    des_input <- input/Dec$seasonal
    SIout <- head(rep(Dec$seasonal[(length(Dec$seasonal)-ppy+1):length(Dec$seasonal)], fh), fh)
  } else {
    des_input <- input; SIout <- rep(1, fh)
  }
  return(naive(des_input, h=fh)$mean*SIout)
}

smape_cal <- function(outsample, forecasts){
  # Used to estimate sMAPE
  outsample <- as.numeric(outsample) ; forecasts<-as.numeric(forecasts)
  smape <- mean((abs(outsample-forecasts)*2)/(abs(outsample)+abs(forecasts)))
  return(smape)
}

mase_cal <- function(insample, outsample, forecasts){
  # Used to estimate MASE
  frq <- frequency(insample)
  forecastsNaiveSD <- rep(NA,frq)
  for (j in (frq+1):length(insample)){
    forecastsNaiveSD <- c(forecastsNaiveSD, insample[j-frq])
  }
  masep<-mean(abs(insample-forecastsNaiveSD),na.rm = TRUE)
  
  outsample <- as.numeric(outsample) ; forecasts <- as.numeric(forecasts)
  mase <- mean((1*abs(outsample-forecasts))/masep)
  return(mase)
}

sOWA_cal <- function(smape, smape_naive_2, mase, mase_naive_2) {
  tol <- 1e-8
  return (0.5*((smape)/(smape_naive_2+tol) + (mase)/(mase_naive_2+tol)))
}


# df <- readRDS('/home/antonio/Dropbox/Metalearning/val/train/series_quarterly.rds')
# p <- read.csv('/home/antonio/PycharmProjects/metalearning/baseline_quarterly_forecasts.csv')
# a <- read.csv('/home/antonio/PycharmProjects/metalearning/baseline_quarterly_average.csv')
# 
# compute_sOWA_metalearner <- function(df_true, df_pred) {
#   n_series <- nrow(df_pred)
#   sOWA_vec <- rep(0, n_series)
#   for (i in 1:n_series) {
#     
#     naive_2_forec <- naive_2(df[[i]]$ts_train, length(as.numeric(df[[i]]$ts_test)))
#     smape <- smape_cal(as.numeric(df_true[[i]]$ts_test), as.numeric(df_pred[i, ]))
#     smape_naive_2 <- smape_cal(as.numeric(df[[i]]$ts_test), as.numeric(naive_2_forec))
#     mase <- mase_cal(df[[i]]$ts_train, df[[i]]$ts_test, as.numeric(df_pred[i, ]))
#     mase_naive_2 <- mase_cal(df[[i]]$ts_train, df[[i]]$ts_test, naive_2_forec)
#     
#     sOWA_vec[i] <- sOWA_cal(smape, smape_naive_2, mase, mase_naive_2)
#   }
#   return (sOWA_vec)
# }
# 
# sOWA_vec <- compute_sOWA_metalearner(df, p)
# sOWA_vec2 <- sOWA_vec[-which(sOWA_vec == Inf)]
# sOWA_average <- compute_sOWA_metalearner(df, a)
# sOWA_average2 <- sOWA_average[-which(sOWA_average == Inf)]

# get_MSE_vec_series <- function(df_errors, i) {
#   h <- ncol(df_errors[[i]]$mat_forecast) # forecasting horizon
#   m <- nrow(df_errors[[i]]$mat_forecast) # number of methods
#   MSE_vec <- rep(0, m)
#   for (j in 1:m) {
#     MSE_vec[j] <- mean((as.numeric(df_errors[[i]]$mat_forecast[j, ]) - as.numeric(df_errors[[i]]$ts_test))^2)
#   }
#   return (MSE_vec)
# }
# 
# get_DIV_mat_series <- function(df_errors, t) {
#   h <- ncol(df_errors[[t]]$mat_forecast) # forecasting horizon
#   m <- nrow(df_errors[[t]]$mat_forecast) # number of methods
#   DIV_mat <- matrix(NA, nrow = m, ncol = m)
#   for (i in 1:m) {
#     for (j in 1:m) {
#       DIV_mat[i, j] <- mean((as.numeric(df_errors[[t]]$mat_forecast[i, ]) - (as.numeric(df_errors[[t]]$mat_forecast[j, ])))^2)
#     }
#   }
#   return (DIV_mat)
# }
# 
# 
# print(get_MSE_vec_series(df_errors, 1))
# DIV_mat <- get_DIV_mat_series(df_errors, 1)

