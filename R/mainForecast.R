source("getM4.R")
source("baseForecasters.R")
source("runForecast.R")
 
# list of forecasting methods
methods <- M4_forec_methods()

SERIES_TYPE = "quarterly"

M4 <- readRDS(file="M4.rds")

if (SERIES_TYPE == "monthly") {
  df_series <- get_M4_monthly(M4)
} else if (SERIES_TYPE == "yearly") {
  df_series <- get_M4_yearly(M4)
} else if (SERIES_TYPE == "quarterly") {
  df_series <- get_M4_quarterly(M4)
} else if (SERIES_TYPE == "weekly") {
  df_series <- get_M4_weekly(M4)
} else if (SERIES_TYPE == "daily") {
  df_series <- get_M4_daily(M4)
} else if (SERIES_TYPE == "hourly") {
  df_series <- get_M4_hourly(M4)
}

print(length(df_series))
remove(M4)

# compute forecasts from df_yearly, df_monthly, etc
log_file <- paste0(getwd(), paste0(paste0("/log_base_forecasters_test_", SERIES_TYPE), ".txt"))
print(log_file)
o_df_series <- calc_forecasts(log_file, df_series, methods, n.cores=14)

#saveRDS(o_df_series, file=paste0(paste0("./forecast_test_", SERIES_TYPE), ".rds"))