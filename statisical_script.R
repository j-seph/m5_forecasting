library(data.table)
#library(fpp3)
library(forecast)
library(foreach)
library(doMC)

# load in data
calendar_df = fread('input/calendar.csv')
train_df = fread('input/sales_train_validation.csv')
price_df = fread('input/sell_prices.csv')
sample_df = fread('input/sample_submission.csv')

# subset train to get id, d_1, d_2, ...
train_sub = train_df[, -c('item_id', 'dept_id', 'cat_id', 'store_id', 'state_id')]
train_sub_id = train_sub[, id]

# create forecast matrix
train_ts = ts(train_sub, frequency = 7)
forecast_intv = 28
forecast_matrix = matrix(NA, nrow = nrow(train_ts), ncol = forecast_intv)

# forecast 28 days in parallel
registerDoMC(detectCores() - 1)
start_time = Sys.time()
forecast_matrix = foreach(i = 1:nrow(train_ts), .combine = rbind, .packages = c('forecast')) %dopar% {
    forecast_matrix = forecast(auto.arima(train_ts[i,]), h = forecast_intv)$mean
}
end_time = Sys.time()
print(end_time - start_time)

# prepare submission
forecast_matrix[forecast_matrix < 0] = 0
colnames(forecast_matrix) = paste0('F', 1:28)
forecast_df = as.data.table(cbind(train_sub_id, forecast_matrix))
setnames(forecast_df, 'train_sub_id', 'id')
submission = forecast_df[sample_df, on=c('id')][, mget(names(forecast_df))]
for (j in seq_len(ncol(submission))) {
    set(submission, which(is.na(submission[[j]])), j, 0)
}

# write submisison
fwrite(submission, 'auto_arima.csv')


rmsse = function(df, h = 28, n) {
    sqrt((1/h) * (sum((df$actual - df$pred) ** 2) / (1/(n - 1)) * sum(diff(df$actual))))
  
}







