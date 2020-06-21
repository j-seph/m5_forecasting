# feature_eng = function(data) {
#     
#     data = copy(data)
#     
#     # categorical features
#     # cols <- c("item_id", "store_id", "state_id", "dept_id", "cat_id") 
#     cols <- c("item_id") 
#     data[, (cols) := lapply(.SD, function(x) as.integer(factor(x))), .SDcols = cols]
#     
#     # time features
#     data[, `:=`(wday = wday(date)
#                 , mday = mday(date)
#                 , week = week(date)
#                 , month = month(date)
#                 , year = year(date)
#                 , quarter = quarter(date)
#                 , yday = yday(date))]
#     
#     # first half of month feature (paychecks arrive first half of month?)
#     data[, first_half_month := fifelse(mday <= 15, 1, 0)]
#     
#     data[, season := dplyr::case_when(month %in% c(12,1,2) ~ 0
#                                       , month %in% c(3:5) ~ 1
#                                       , month %in% c(6:8) ~ 2
#                                       , month %in% c(9:11) ~ 4)]
#     
#     # lag features
#     lapply(c(1, 7, 28), function(x) {
#         data[, paste0('lag_', x, '_sales') := shift(sales, x), by = .(item_id, store_id)]
#     })
#     
#     # rolling agg features
#     lapply(c(7, 28), function(x) {
#         data[, paste0('rmax_', x, '_sales') := shift(frollapply(sales, x, FUN = max, na.rm = TRUE)), by = .(item_id, store_id)]
#     })
#     
#     # lapply(c(7), function(x) {
#     #     data[, paste0('rsd_', x, '_sales') := frollapply(shift(sales), x, FUN = sd, na.rm = TRUE), by = .(item_id, store_id)]
#     # })
#     
#     data[, rsd_7_sales := shift(frollapply(sales, 7, FUN = sd, na.rm = TRUE)), by = .(item_id, store_id)]
#     
#     # rolling mean of lag dow features
#     lapply(c(2, 4), function(x) {
#         data[, paste0('rmax_', x, '_dow') := shift(frollapply(sales, x, FUN = max, na.rm=TRUE)), by = .(item_id, store_id, wday)]
#     })
#     
#     lapply(c(2, 4), function(x) {
#         data[, paste0('rmin_', x, '_dow') := shift(frollapply(sales, x, FUN = min, na.rm=TRUE)), by = .(item_id, store_id, wday)]
#     })
#     
#     # lapply(c(2, 4), function(x) {
#     #     data[, paste0('rsd_', x, '_dow') := shift(frollapply(sales, x, FUN = sd, na.rm=TRUE)), by = .(item_id, store_id, wday)]
#     # })
#     
#     # rolling mean of lag features
#     lag_cols = names(data)[grepl('lag', names(data))]
#     
#     lapply(c(7, 28), function(x) {
#         data[, paste0('rmean_', x,'_', lag_cols) := frollmean(.SD, x, na.rm = TRUE), by = .(item_id, store_id), .SDcols = lag_cols]
#     })
# 
#     # trend features (avg daily sales / avg sales over history)
#     data[, rmean_7_sales := shift(frollmean(sales, 7, na.rm = TRUE)), by = .(item_id, store_id)]
# 
#     data[, rmean_7_lag_1_sales_ratio := lag_1_sales / rmean_7_sales]
#     data[is.na(rmean_7_lag_1_sales_ratio), rmean_7_lag_1_sales_ratio := 0]
#     
#     # data[, rmean_7_lag_1_sales_diff := lag_1_sales - rmean_7_sales]
# 
#     data[, rmean_7_sales := NULL]
#     
#     data[, rmax_7_lag_1_sales_ratio := lag_1_sales / rmax_7_sales]
#     data[is.na(rmax_7_lag_1_sales_ratio), rmax_7_lag_1_sales_ratio := 0]
#     
#     data[, rsd_7_lag_1_sales_ratio := lag_1_sales / rsd_7_sales]
#     data[is.na(rsd_7_lag_1_sales_ratio), rsd_7_lag_1_sales_ratio := 0]
#     
#     # holiday features
#     data = data[event_name_1 == 'Christmas', .(last_christmas = date), key = 'date'][data, on = 'date', roll = TRUE]
#     data[, days_since_christmas := date - last_christmas]
#     data[, last_christmas := NULL]
# 
#     data = data[event_name_1 == 'Easter', .(last_easter = date), key = 'date'][data, on = 'date', roll = TRUE]
#     data[, days_since_easter := date - last_easter]
#     data[, last_easter := NULL]
#     
#     data = data[event_name_1 == 'SuperBowl', .(last_superbowl = date), key = 'date'][data, on = 'date', roll = TRUE]
#     data[, days_since_superbowl := date - last_superbowl]
#     data[, last_superbowl := NULL]
#     
#     data = data[event_name_1 == 'IndependenceDay', .(last_independenceday = date), key = 'date'][data, on = 'date', roll = TRUE]
#     data[, days_since_independenceday := date - last_independenceday]
#     data[, last_independenceday := NULL]
#     
#     data = data[event_name_1 == 'Thanksgiving', .(last_thanksgiving = date), key = 'date'][data, on = 'date', roll = TRUE]
#     data[, days_since_thanksgiving := date - last_thanksgiving]
#     data[, last_thanksgiving := NULL]
#     
#     data = data[event_name_1 == 'Halloween', .(last_halloween = date), key = 'date'][data, on = 'date', roll = TRUE]
#     data[, days_since_halloween := date - last_halloween]
#     data[, last_halloween := NULL]
#     
#     data = data[event_name_1 == 'NewYear', .(last_newyear = date), key = 'date'][data, on = 'date', roll = TRUE]
#     data[, days_since_newyear := date - last_newyear]
#     data[, last_newyear := NULL]
#     
#     data[, event_name_1 := as.integer(as.factor(event_name_1))]
#     # event_dummy = dcast(dt, item_id + store_id + d ~ event_name_1, fun.aggregate = length, value.var = 'event_name_1')
#     # event_dummy[, V1 := NULL]
#     # dt = event_dummy[dt, on = c('item_id', 'store_id', 'd')]
#     # dt[, event_name_1 := NULL]
#     # rm(event_dummy)
#     
#     # expanding window features
#     
#     # price features
#     # price momentums
#     sell_prices = data[, .(sell_price = unique(sell_price)), .(item_id, store_id, wm_yr_wk, month, year)][order(item_id, store_id, wm_yr_wk)]
#     sell_prices = sell_prices[!duplicated(sell_prices, by = c('wm_yr_wk', 'item_id', 'store_id', 'sell_price', 'year'))]
#     sell_prices[, lw_sell_price := shift(sell_price), by = .(item_id, store_id)]
#     sell_prices[, weekly_price_momentum := sell_price/lw_sell_price, by = .(item_id, store_id)]
# 
#     avg_monthly_sell_prices = sell_prices[, .(sell_price = mean(sell_price)), by = .(item_id, store_id, month)]
#     avg_monthly_sell_prices[, lm_sell_price := shift(sell_price), by = .(item_id, store_id)]
#     avg_monthly_sell_prices[, monthly_price_momentum := sell_price/lm_sell_price]
#     sell_prices[avg_monthly_sell_prices, monthly_price_momentum := i.monthly_price_momentum, on = c('item_id', 'store_id', 'month')]
# 
#     avg_yearly_sell_prices = sell_prices[, .(sell_price = mean(sell_price)), by = .(item_id, store_id, year)]
#     avg_yearly_sell_prices[, ly_sell_price := shift(sell_price), by = .(item_id, store_id)]
#     avg_yearly_sell_prices[, yearly_price_momentum := sell_price/ly_sell_price]
#     sell_prices[avg_yearly_sell_prices, yearly_price_momentum := i.yearly_price_momentum, on = c('item_id', 'store_id', 'year')]
# 
#     data[sell_prices, `:=` (weekly_price_momentum = i.weekly_price_momentum
#                           , monthly_price_momentum = i.monthly_price_momentum
#                           , yearly_price_momentum = i.yearly_price_momentum
#                           ), on = c('item_id', 'store_id', 'wm_yr_wk')]
# 
#     # price summary stats by year
#     data[, `:=` (sell_price_year_max = max(sell_price, na.rm = TRUE)
#                , sell_price_year_min = min(sell_price, na.rm = TRUE)
#                , sell_price_year_sd = sd(sell_price, na.rm = TRUE)
#                , sell_price_year_mean = mean(sell_price, na.rm = TRUE)
#                , sell_price_year_median = median(sell_price, na.rm = TRUE)), by = .(item_id, store_id, year)]
#     
#     # price summary stats by season
#     # data[, `:=` (sell_price_season_max = max(sell_price, na.rm = TRUE)
#     #              , sell_price_season_min = min(sell_price, na.rm = TRUE)
#     #              , sell_price_season_sd = sd(sell_price, na.rm = TRUE)
#     #              , sell_price_season_mean = mean(sell_price, na.rm = TRUE)
#     #              , sell_price_season_median = median(sell_price, na.rm = TRUE)), by = .(item_id, store_id, season)]
# 
#     # extra price features
#     # data[, `:=` (sell_price_pspy_uniqueN = uniqueN(sell_price)), by = .(item_id, store_id, year)]
#     # data[, `:=` (sell_price_psps_uniqueN = uniqueN(sell_price)), by = .(item_id, store_id, season)]
#     # data[, `:=` (sell_price_pspm_uniqueN = uniqueN(sell_price)), by = .(item_id, store_id, month)]
#     
#     # # replace Inf with NA then omit NA
#     # for (j in 1:ncol(data)) set(data, which(is.infinite(data[[j]])), j, NA)
#     # data = na.omit(data) # experiment with/without
#     
#     data[, `:=` (store_id = NULL
#                  , state_id = NULL
#                  , dept_id = NULL
#                  , cat_id = NULL)]
#     
#     return(data)
# }

feature_eng = function(data) {
    
    data = copy(data)
    
    # categorical features
    cols <- c("item_id") 
    data[, (cols) := lapply(.SD, function(x) as.integer(factor(x))), .SDcols = cols]
    
    # time features
    data[, `:=`(wday = wday(date)
                , mday = mday(date)
                , week = week(date)
                , month = month(date)
                , year = year(date)
                , quarter = quarter(date)
                , yday = yday(date))]
    
    # first half of month feature (paychecks arrive first half of month?)
    data[, first_half_month := fifelse(mday <= 15, 1, 0)]
    
    data[, season := dplyr::case_when(month %in% c(12,1,2) ~ 0
                                      , month %in% c(3:5) ~ 1
                                      , month %in% c(6:8) ~ 2
                                      , month %in% c(9:11) ~ 4)]
    
    # lag features
    lapply(c(7, 28), function(x) {
        data[, paste0('lag_', x, '_sales') := shift(sales, x), by = .(item_id)]
    })
    
    # # rolling agg features
    # lapply(c(7, 28), function(x) {
    #     data[, paste0('rmax_', x, '_sales') := shift(frollapply(sales, x, FUN = max, na.rm = TRUE)), by = .(item_id)]
    # })
    # 
    # lapply(c(7), function(x) {
    #     data[, paste0('rsd_', x, '_sales') := shift(frollapply(sales, x, FUN = sd, na.rm = TRUE)), by = .(item_id)]
    # })
    # 
    # data[, rsd_7_sales := shift(frollapply(sales, 7, FUN = sd, na.rm = TRUE)), by = .(item_id)]
    # 
    # # rolling mean of lag dow features
    # lapply(c(2, 4), function(x) {
    #     data[, paste0('rmax_', x, '_dow') := shift(frollapply(sales, x, FUN = max, na.rm=TRUE)), by = .(item_id, wday)]
    # })
    # 
    # lapply(c(2, 4), function(x) {
    #     data[, paste0('rsd_', x, '_dow') := shift(frollapply(sales, x, FUN = sd, na.rm=TRUE)), by = .(item_id, wday)]
    # })
    
    # rolling mean of lag features
    lag_cols = names(data)[grepl('lag', names(data))]
    
    lapply(c(7, 28), function(x) {
        data[, paste0('rmean_', x,'_', lag_cols) := frollmean(.SD, x, na.rm = TRUE), by = .(item_id), .SDcols = lag_cols]
    })
    
    # # trend features (avg daily sales / avg sales over history)
    # data[, rmean_7_sales := shift(frollmean(sales, 7, na.rm = TRUE)), by = .(item_id)]
    # 
    # data[, rmean_7_lag_1_sales_ratio := lag_1_sales / rmean_7_sales]
    # data[is.na(rmean_7_lag_1_sales_ratio), rmean_7_lag_1_sales_ratio := 0]
    # 
    # # data[, rmean_7_lag_1_sales_diff := lag_1_sales - rmean_7_sales]
    # 
    # data[, rmean_7_sales := NULL]
    # 
    # data[, rmax_7_lag_1_sales_ratio := lag_1_sales / rmax_7_sales]
    # data[is.na(rmax_7_lag_1_sales_ratio), rmax_7_lag_1_sales_ratio := 0]
    # 
    # data[, rsd_7_lag_1_sales_ratio := lag_1_sales / rsd_7_sales]
    # data[is.na(rsd_7_lag_1_sales_ratio), rsd_7_lag_1_sales_ratio := 0]
    # 
    # # holiday features
    # data = data[event_name_1 == 'Christmas', .(last_christmas = date), key = 'date'][data, on = 'date', roll = TRUE]
    # data[, days_since_christmas := date - last_christmas]
    # data[, last_christmas := NULL]
    # 
    # data = data[event_name_1 == 'Easter', .(last_easter = date), key = 'date'][data, on = 'date', roll = TRUE]
    # data[, days_since_easter := date - last_easter]
    # data[, last_easter := NULL]
    # 
    # data = data[event_name_1 == 'SuperBowl', .(last_superbowl = date), key = 'date'][data, on = 'date', roll = TRUE]
    # data[, days_since_superbowl := date - last_superbowl]
    # data[, last_superbowl := NULL]
    # 
    # data = data[event_name_1 == 'IndependenceDay', .(last_independenceday = date), key = 'date'][data, on = 'date', roll = TRUE]
    # data[, days_since_independenceday := date - last_independenceday]
    # data[, last_independenceday := NULL]
    # 
    # data = data[event_name_1 == 'Thanksgiving', .(last_thanksgiving = date), key = 'date'][data, on = 'date', roll = TRUE]
    # data[, days_since_thanksgiving := date - last_thanksgiving]
    # data[, last_thanksgiving := NULL]
    # 
    # data = data[event_name_1 == 'Halloween', .(last_halloween = date), key = 'date'][data, on = 'date', roll = TRUE]
    # data[, days_since_halloween := date - last_halloween]
    # data[, last_halloween := NULL]
    # 
    # data = data[event_name_1 == 'NewYear', .(last_newyear = date), key = 'date'][data, on = 'date', roll = TRUE]
    # data[, days_since_newyear := date - last_newyear]
    # data[, last_newyear := NULL]
    # 
    # data[, event_name_1 := as.integer(as.factor(event_name_1))]
    # 
    # # expanding window features
    # 
    # # price features
    # # price momentums
    # sell_prices = data[, .(sell_price = unique(sell_price)), .(item_id, wm_yr_wk, month, year)][order(item_id, wm_yr_wk)]
    # sell_prices = sell_prices[!duplicated(sell_prices, by = c('wm_yr_wk', 'item_id', 'sell_price', 'year'))]
    # sell_prices[, lw_sell_price := shift(sell_price), by = .(item_id)]
    # sell_prices[, weekly_price_momentum := sell_price/lw_sell_price, by = .(item_id)]
    # 
    # avg_monthly_sell_prices = sell_prices[, .(sell_price = mean(sell_price)), by = .(item_id, month)]
    # avg_monthly_sell_prices[, lm_sell_price := shift(sell_price), by = .(item_id)]
    # avg_monthly_sell_prices[, monthly_price_momentum := sell_price/lm_sell_price]
    # sell_prices[avg_monthly_sell_prices, monthly_price_momentum := i.monthly_price_momentum, on = c('item_id', 'month')]
    # 
    # avg_yearly_sell_prices = sell_prices[, .(sell_price = mean(sell_price)), by = .(item_id, year)]
    # avg_yearly_sell_prices[, ly_sell_price := shift(sell_price), by = .(item_id)]
    # avg_yearly_sell_prices[, yearly_price_momentum := sell_price/ly_sell_price]
    # sell_prices[avg_yearly_sell_prices, yearly_price_momentum := i.yearly_price_momentum, on = c('item_id', 'year')]
    # 
    # data[sell_prices, `:=` (weekly_price_momentum = i.weekly_price_momentum
    #                         , monthly_price_momentum = i.monthly_price_momentum
    #                         , yearly_price_momentum = i.yearly_price_momentum
    # ), on = c('item_id', 'wm_yr_wk')]
    # 
    # # price summary stats by year
    # data[, `:=` (sell_price_year_max = max(sell_price, na.rm = TRUE)
    #              , sell_price_year_min = min(sell_price, na.rm = TRUE)
    #              , sell_price_year_sd = sd(sell_price, na.rm = TRUE)
    #              , sell_price_year_mean = mean(sell_price, na.rm = TRUE)
    #              , sell_price_year_median = median(sell_price, na.rm = TRUE)), by = .(item_id, year)]
    
    # price summary stats by season
    # data[, `:=` (sell_price_season_max = max(sell_price, na.rm = TRUE)
    #              , sell_price_season_min = min(sell_price, na.rm = TRUE)
    #              , sell_price_season_sd = sd(sell_price, na.rm = TRUE)
    #              , sell_price_season_mean = mean(sell_price, na.rm = TRUE)
    #              , sell_price_season_median = median(sell_price, na.rm = TRUE)), by = .(item_id, store_id, season)]
    
    # extra price features
    # data[, `:=` (sell_price_pspy_uniqueN = uniqueN(sell_price)), by = .(item_id, store_id, year)]
    # data[, `:=` (sell_price_psps_uniqueN = uniqueN(sell_price)), by = .(item_id, store_id, season)]
    # data[, `:=` (sell_price_pspm_uniqueN = uniqueN(sell_price)), by = .(item_id, store_id, month)]
    
    # # replace Inf with NA then omit NA
    # for (j in 1:ncol(data)) set(data, which(is.infinite(data[[j]])), j, NA)
    # data = na.omit(data) # experiment with/without
    
    data[, `:=` (store_id = NULL
                 , state_id = NULL
                 , dept_id = NULL
                 , cat_id = NULL)]
    
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








