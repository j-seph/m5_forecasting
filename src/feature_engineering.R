feature_eng = function(data) {
    
    data = copy(data)
    
    # categorical features
    cols <- c("item_id", "store_id", "state_id", "dept_id", "cat_id") 
    data[, (cols) := lapply(.SD, function(x) as.integer(factor(x))), .SDcols = cols]
    
    # lag features
    lapply(c(1, 7, 28), function(x) {
        data[, paste0('lag_', x, '_sales') := shift(sales, x), by = .(item_id, store_id)]
    })
    
    # # rolling mean features
    # lapply(c(7, 28), function(x) {
    #     data[, paste0('rmean', x, '_sales') := frollmean(sales, x, na.rm = TRUE), by = .(item_id, store_id)]
    # })
    
    # rolling mean of lag features
    lag_cols = names(data)[grepl('lag', names(data))]
    
    lapply(c(7, 28), function(x) {
        data[, paste0('rmean_', x,'_', lag_cols) := frollmean(.SD, x, na.rm = TRUE), by = .(item_id, store_id), .SDcols = lag_cols]
    })
    
    # data[, rmean_7_sales := frollmean(shift(sales), 7, na.rm = TRUE), by = .(item_id, store_id)]
    # 
    # data[, last_7_days_lag_1_sales_ratio := lag_1_sales / rmean_7_sales]
    # 
    # data[, rmean_7_sales := NULL]
    
    # time features
    data[, `:=`(wday = wday(date),
              mday = mday(date),
              week = week(date),
              month = month(date),
              year = year(date))]
    
    data[, season := dplyr::case_when(month %in% c(12,1,2) ~ 0
                                      , month %in% c(3:5) ~ 1
                                      , month %in% c(6:8) ~ 2
                                      , month %in% c(9:11) ~ 4)]
    
    # holiday features
    data = data[event_name_1 == 'Christmas', .(last_christmas = date), key = 'date'][data, on = 'date', roll = TRUE]
    data[, days_since_christmas := date - last_christmas]
    data[, last_christmas := NULL]

    data = data[event_name_1 == 'Easter', .(last_easter = date), key = 'date'][data, on = 'date', roll = TRUE]
    data[, days_since_easter := date - last_easter]
    data[, last_easter := NULL]
    
    data = data[event_name_1 == 'SuperBowl', .(last_superbowl = date), key = 'date'][data, on = 'date', roll = TRUE]
    data[, days_since_superbowl := date - last_superbowl]
    data[, last_superbowl := NULL]
    
    data = data[event_name_1 == 'IndependenceDay', .(last_independenceday = date), key = 'date'][data, on = 'date', roll = TRUE]
    data[, days_since_independenceday := date - last_independenceday]
    data[, last_independenceday := NULL]
    
    data = data[event_name_1 == 'Thanksgiving', .(last_thanksgiving = date), key = 'date'][data, on = 'date', roll = TRUE]
    data[, days_since_thanksgiving := date - last_thanksgiving]
    data[, last_thanksgiving := NULL]
    
    data = data[event_name_1 == 'Halloween', .(last_halloween = date), key = 'date'][data, on = 'date', roll = TRUE]
    data[, days_since_halloween := date - last_halloween]
    data[, last_halloween := NULL]
    
    data = data[event_name_1 == 'NewYear', .(last_newyear = date), key = 'date'][data, on = 'date', roll = TRUE]
    data[, days_since_newyear := date - last_newyear]
    data[, last_newyear := NULL]
    
    data[, event_name_1 := as.integer(as.factor(event_name_1))]
    # event_dummy = dcast(dt, item_id + store_id + d ~ event_name_1, fun.aggregate = length, value.var = 'event_name_1')
    # event_dummy[, V1 := NULL]
    # dt = event_dummy[dt, on = c('item_id', 'store_id', 'd')]
    # dt[, event_name_1 := NULL]
    # rm(event_dummy)
    
    # expanding window features
    
    # trend features (avg daily sales / avg sales over history)
    
    # # rolling mean of lag dow features
    # data = data[order(item_id, store_id, date)]

    # lapply(c(2, 4), function(x) {
    #     data[, paste0('rmean_', x, '_dow') := frollmean(shift(sales), x, na.rm=TRUE), by = .(item_id, store_id, wday)]
    # })
    # 
    # # price features
    # # price momentums
    # sell_prices = data[, .(sell_price = unique(sell_price)), .(item_id, store_id, wm_yr_wk, month, year)][order(item_id, store_id, wm_yr_wk)]
    # sell_prices = sell_prices[!duplicated(sell_prices, by = c('wm_yr_wk', 'item_id', 'store_id', 'sell_price', 'year'))]
    # sell_prices[, lw_sell_price := shift(sell_price), by = .(item_id, store_id)]
    # sell_prices[, weekly_price_momentum := sell_price/lw_sell_price, by = .(item_id, store_id)]
    # 
    # sell_prices[, avg_month_sell_price := mean(sell_price), by = .(item_id, store_id, month)]
    # sell_prices[, lm_avg_sell_price := shift(avg_month_sell_price), by = .(item_id, store_id)]
    # sell_prices[, monthly_price_momentum := avg_month_sell_price/lm_avg_sell_price]
    # 
    # sell_prices[, avg_year_sell_price := mean(sell_price), by = .(item_id, store_id, month)]
    # sell_prices[, ly_avg_sell_price := shift(avg_year_sell_price), by = .(item_id, store_id)]
    # sell_prices[, yearly_price_momentum := avg_year_sell_price/ly_avg_sell_price]
    # 
    # data[sell_prices, `:=` (weekly_price_momentum = i.weekly_price_momentum
    #                       , monthly_price_momentum = i.monthly_price_momentum
    #                       , yearly_price_momentum = i.yearly_price_momentum), on = c('item_id', 'store_id', 'wm_yr_wk')]
    # 
    # # price summary stats
    # data[, `:=` (sell_price_year_max = max(sell_price, na.rm = TRUE)
    #            , sell_price_year_min = min(sell_price, na.rm = TRUE)
    #            , sell_price_year_sd = sd(sell_price, na.rm = TRUE)
    #            , sell_price_mean = mean(sell_price, na.rm = TRUE)
    #            , sell_price_median = median(sell_price, na.rm = TRUE)), by = .(item_id, store_id, year)]
    # 
    # # extra price features
    # data[, `:=` (sell_price_year_uniqueN = uniqueN(sell_price)), by = .(item_id, year)]
    
    return(data)
}

# create_time_features = function(data) {
#     data = copy(data)
# 
#     data[, date := as.IDate(date)]
# 
#     data[, season := dplyr::case_when(month %in% c(12, 1, 2) ~ 1,
#                                       month %in% c(3, 4, 5) ~ 2,
#                                       month %in% c(6, 7, 8) ~ 3,
#                                       month %in% c(9, 10, 11) ~ 4)]
# 
#     data = data[event_name_1 == 'Christmas', .(last_christmas = date), key = 'date'][data, on = 'date', roll = TRUE]
#     data[, days_since_christmas := date - last_christmas]
#     data[, last_christmas := NULL]
# 
#     data = data[event_name_1 == 'Easter', .(last_easter = date), key = 'date'][data, on = 'date', roll = TRUE]
#     data[, days_since_easter := date - last_easter]
#     data[, last_easter := NULL]
#     
#     
# 
#     return(data)
# }





