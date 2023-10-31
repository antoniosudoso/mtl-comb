source("getM4.R")

SERIES_TYPE <- "quarterly"

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

# read original M4 dataset with train and test split
M4 <- readRDS(file="M4.rds")

if (SERIES_TYPE == "monthly") {
  df_series <- get_M4_monthly(M4)
} else if (SERIES_TYPE == "yearly") {
  df_series <- get_M4_yearly(M4)
} else if (SERIES_TYPE == "quarterly") {
  df_series <- get_M4_quarterly(M4)
}

print(length(df_series))
remove(M4)

# read forecasts for the test part of the M4 dataset
f_series <- readRDS(paste0(paste0("forecast_test_", SERIES_TYPE), ".rds"))
df <- create_test_series_data(df_series, f_series)
# main .rds for testing neural network (needed for computing final tables)
# saveRDS(df, file=paste0(paste0("series_test_", SERIES_TYPE), ".rds"))
