# load in datasets
calendar_raw = fread('/home/prometheus/workspace/kaggle/m5_forecasting/input/calendar.csv')
train_raw = fread('/home/prometheus/workspace/kaggle/m5_forecasting/input/sales_train_validation.csv')
price_raw = fread('/home/prometheus/workspace/kaggle/m5_forecasting/input/sell_prices.csv')

# merge
train_m = melt.data.table(train, measure.vars = patterns('^d_'), variable.name = 'd', value.name = 'sales', variable.factor = FALSE)
train_m = calendar_raw[train_m, on=c('d')]
train_m = price_raw[train_m, on=c('store_id', 'item_id', 'wm_yr_wk')]

rm(calendar_raw, train_raw, price_raw)
invisible(gc())


