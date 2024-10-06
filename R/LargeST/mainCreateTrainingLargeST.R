setwd('../')


create_training_series_data <- function(df_series, qp_labels) {
  if (length(df_series) != nrow(qp_labels)) {
    stop('Incompatible length')
  }
  n <- length(df_series)
  df <- list()
  for (i in 1:n) {
    lentry <- list()
    lentry$ts_train <- as.numeric(df_series[[i]]$ts_train)
    lentry$ts_test <- as.numeric(df_series[[i]]$ts_test)
    lentry$mat_forecast <- as.matrix(df_series[[i]]$mat_forecast)
    lentry$div_label <- as.numeric(qp_labels[i, ])
    df[[i]] <- lentry
  }
  return (df)
}

SERIES_TYPE <- "Hourly"

df_series <- readRDS(paste0(paste0("./LargeST/data/raw_labels_", SERIES_TYPE), ".rds"))
qp_labels <- readRDS(paste0(paste0('./LargeST/data/qp_labels_', SERIES_TYPE), '.rds'))
df_series <- create_training_series_data(df_series, qp_labels)

print(length(df_series))

# main .rds for training neural network
saveRDS(df_series, file=paste0(paste0("./LargeST/series_", SERIES_TYPE), ".rds"))


