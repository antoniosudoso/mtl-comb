library(forecast)
library(stringr)

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
  mase <- mean((2*abs(outsample-forecasts))/masep)
  return(mase)
}

sOWA_cal <- function(smape, smape_naive_2, mase, mase_naive_2) {
  tol <- 1e-8
  return (0.5*((smape)/(smape_naive_2+tol) + (mase)/(mase_naive_2+tol)))
}
