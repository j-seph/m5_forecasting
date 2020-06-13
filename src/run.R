library(data.table)
library(xgboost)
library(fst)
library(caret)

source('src/dataset.R')
source('src/feature_engineering.R')
source('src/models.R')
source('src/train.R')
source('src/predict.R')


config = list(
    input_path = 'src/input/'
    , output_path = 'src/output/'
    , model_path = 'src/model/'
    , partition = 'CA'
    , h = 28
    , train_end_date = as.IDate('2016-03-27')
    , test_start_date = as.IDate('2016-04-25')
    , max_lags = 365
    , tr_last = 1913
    , features = c("id", "item_id", "dept_id", "cat_id", "store_id", "state_id", 
                   "sales", "date", "event_name_1", "snap_CA", 
                   "snap_TX", "snap_WI", "sell_price", "lag_7_sales", "lag_28_sales", 
                   "rmean7_sales", "rmean28_sales", "rmean_7_lag_7_sales", "rmean_7_lag_28_sales", 
                   "rmean_28_lag_7_sales", "rmean_28_lag_28_sales", "wday", "mday", 
                   "week", "month", "year", "rmean_4_dow", "rmean_12_dow", "weekly_price_momentum", 
                   "monthly_price_momentum", "yearly_price_momentum", "sell_price_year_max", 
                   "sell_price_year_min", "sell_price_year_sd", "sell_price_mean", 
                   "sell_price_median", "sell_price_year_uniqueN")
    , saved_model = paste0('src/model/xgb_', Sys.Date())
)

# create train data
dt = create_dt(is_train = TRUE, config = config)

# feature engineering
dt = feature_eng(dt)

dt = na.omit(dt) # experiment with/without

# train the model
train_model(dt, config)

# examine feature importance
xgb_model = xgb.load(config$saved_model)
mat <- xgb.importance(feature_names = colnames(dt[, -c('id', 'sales', 'date', 'd', 'wm_yr_wk')]), model = xgb_model)
xgb.plot.importance(importance_matrix = mat)

# create test data
test_dt = create_dt(is_train = FALSE, config)

test_dt = na.omit(test_dt)

# predict
test_dt = predict_sales(test_dt, config)

# evaluate
test_dt[, day_idnt := as.integer(gsub('d_', '', d))]
prediction = test_dt[day_idnt >= 1914 & day_idnt <= 1941, .(d, item_id, store_id, sales)]
prediction[, d := as.character(d)]
prediction[, preds := sales]
prediction[, sales := NULL]

evaluation = fread('src/input/sales_train_evaluation.csv')
evaluation = melt(evaluation,
                  measure.vars = patterns('^d_'),
                  variable.name = 'd',
                  value.name = 'sales')
evaluation[, d := as.character(d)]

#prediction[, id := sub('validation', 'evaluation', id)]

prediction = evaluation[prediction, on=c('item_id', 'store_id', 'd')]
rm(evaluation)

Metrics::rmse(prediction[, sales], prediction[, preds])

prediction[, d_ := as.numeric(gsub('d_', '', d))]
test = prediction[, .(sales = sum(sales), preds = sum(preds)), by = .(d_)]
test = melt(test, 'd_')
ggplot(test, aes(x = d_, y = value, color = variable)) +
    geom_line()

# CA base 2.145412 more accurate at start of the horizon
# CA lag 1 2.188809 more accurate as horizon increases
# CA lag 1 + season 2.171115 
# CA lag 1 + season + ohe event_name_type 2.188919
# CA lag 1 + season + days since holiday features 2.166567
# CA lag 1 + rmean7&28 3.549032 severely under-predicts
# CA lag 1 + rmean7&28 dow 3.770984 moderately under-predicts 
# CA lag 1 + rmean2&4 dow 3.762374 slightly better than longer dow rmeans 3.762374
# CA lag 1 + last_7_days_lag_1_sales_ratio 3.767034 




