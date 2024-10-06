setwd('../')

# specify series type: yearly, quarterly and monthly
SERIES_TYPE <- "quarterly"

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

find_constant_series <- function(df_series) {
  n <- length(df_series)
  ids <- list()
  n_ids <- 1
  for (i in 1:n) {
    if (var(as.numeric(df_series[[i]]$ts_train)) < 1e-8 || var(as.numeric(df_series[[i]]$ts_test)) < 1e-8) {
      ids[[n_ids]] <- i
      n_ids <- n_ids + 1
    }
  }
  return (unlist(ids))
}

df_series <- readRDS(paste0(paste0("val/raw_labels_", SERIES_TYPE), ".rds"))
qp_labels <- readRDS(paste0(paste0('val/train/qp_labels_', SERIES_TYPE), '.rds'))
df_series <- create_training_series_data(df_series, qp_labels)
remove_id <- find_constant_series(df_series)
print(remove_id)

df_series <- df_series[-remove_id]
print(length(df_series))

# main .rds for training neural network
#saveRDS(df_series, file=paste0(paste0("val/train/series_", SERIES_TYPE), ".rds"))




