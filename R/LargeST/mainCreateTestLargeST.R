setwd('../')


create_test_series_data <- function(df_series, f_series) {
  if (length(df_series) != length(f_series)) {
    stop('Incompatible length')
  }
  n <- length(df_series)
  df <- list()
  for (i in 1:n) {
    lentry <- list()
    lentry$ts_train <- as.numeric(df_series[[i]]$x)
    lentry$ts_test <- as.numeric(df_series[[i]]$xx)
    lentry$mat_forecast <- as.matrix(f_series[[i]])
    df[[i]] <- lentry
  }
  return (df)
}

SERIES_TYPE <- "Hourly"

df_series <- readRDS(file=paste0(paste0(paste0("./LargeST/LargeST_test_"), SERIES_TYPE), ".rds"))
print(length(df_series))
#df_series <- df_series[1:12000]
print(length(df_series))
f_series <- readRDS(paste0(paste0("./LargeST/data/forecast_test_", SERIES_TYPE), ".rds"))
df <- create_test_series_data(df_series, f_series)
# main .rds for testing neural network (needed for computing final tables)
saveRDS(df, file=paste0(paste0("./LargeST/series_test_", SERIES_TYPE), ".rds"))