setwd('../')

source("getM4.R")
source("baseForecasters.R")
source("runForecast.R")

# list of forecasting methods
methods <- M4_forec_methods()

SERIES_TYPE = "Hourly"

df_series <- readRDS(file=paste0(paste0(paste0("./LargeST/LargeST_test_"), SERIES_TYPE), ".rds"))

print(length(df_series))

# compute forecasts from df_yearly, df_monthly, etc
log_file <- paste0(getwd(), paste0(paste0("/LargeST/log_base_forecasters_test_", SERIES_TYPE), ".txt"))
print(log_file)
o_df_series <- calc_forecasts(log_file, df_series, methods, n.cores=14)

saveRDS(o_df_series, file=paste0(paste0("./LargeST/forecast_test_", SERIES_TYPE), ".rds"))