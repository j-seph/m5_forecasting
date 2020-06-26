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
    lapply(c(7, 28, 56, 84, 122), function(x) {
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
    # rolling mean of lag dow features
    lapply(c(2, 4), function(x) {
        data[, paste0('rmax_', x, '_dow') := shift(frollapply(sales, x, FUN = max, na.rm=TRUE)), by = .(item_id, wday)]
    })
    

    lapply(c(4), function(x) {
        data[, paste0('rsd_', x, '_dow') := shift(frollapply(sales, x, FUN = sd, na.rm=TRUE)), by = .(item_id, wday)]
    })
    
    # rolling mean of lag features
    lag_cols = names(data)[grepl('lag', names(data))]
    
    lapply(c(7, 28, 56, 84, 122), function(x) {
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
    
    # days since non zero sales
    data[sales != 0, date_ := date]
    data[, id_ := paste(item_id, date, sep = '_')]
    data = data[!is.na(date_), .(non_zero_sales_date = date_), key = c('id_')][data, on=c('id_'), roll=Inf]
    data[, non_zero_sales_date := shift(non_zero_sales_date), .(item_id)]
    data[, days_since_non_zero_sales := date - non_zero_sales_date]
    data[, `:=` (date_ = NULL, id_ = NULL, non_zero_sales_date = NULL)]
    
    # holiday features
    # major holidays
    data = data[event_name_1 == 'Christmas', .(next_christmas = date), key = 'date'][data, on = 'date', roll = -Inf]
    data[, days_to_christmas := next_christmas - date]
    data[days_to_christmas > 14, days_to_christmas := 99]
    data[is.na(days_to_christmas), days_to_christmas := 99]
    data[, next_christmas := NULL]

    data = data[event_name_1 == 'Easter', .(next_easter = date), key = 'date'][data, on = 'date', roll = -Inf]
    data[, days_to_easter := next_easter - date]
    data[days_to_easter > 14, days_to_easter := 99]
    data[is.na(days_to_easter), days_to_easter := 99]
    data[, next_easter := NULL]
    

    data = data[event_name_1 == 'SuperBowl', .(next_superbowl = date), key = 'date'][data, on = 'date', roll = -Inf]
    data[, days_to_superbowl := next_superbowl - date]
    data[days_to_superbowl > 14, days_to_superbowl := 99]
    data[is.na(days_to_superbowl), days_to_superbowl := 99]
    data[, next_superbowl := NULL]

    data = data[event_name_1 == 'IndependenceDay', .(next_independenceday = date), key = 'date'][data, on = 'date', roll = -Inf]
    data[, days_to_independenceday := next_independenceday - date]
    data[days_to_independenceday > 14, days_to_independenceday := 99]
    data[is.na(days_to_independenceday), days_to_independenceday := 99]
    data[, next_independenceday := NULL]

    data = data[event_name_1 == 'Thanksgiving', .(next_thanksgiving = date), key = 'date'][data, on = 'date', roll = -Inf]
    data[, days_to_thanksgiving := next_thanksgiving - date]
    data[days_to_thanksgiving > 14, days_to_thanksgiving := 99]
    data[is.na(days_to_thanksgiving), days_to_thanksgiving := 99]
    data[, next_thanksgiving := NULL]

    data = data[event_name_1 == 'Halloween', .(next_halloween = date), key = 'date'][data, on = 'date', roll = -Inf]
    data[, days_to_halloween := next_halloween - date]
    data[days_to_halloween > 14, days_to_halloween := 99]
    data[is.na(days_to_halloween), days_to_halloween := 99]
    data[, next_halloween := NULL]

    data = data[event_name_1 == 'NewYear', .(next_newyear = date), key = 'date'][data, on = 'date', roll = -Inf]
    data[, days_to_newyear := next_newyear - date]
    data[days_to_newyear > 14, days_to_newyear := 99]
    data[is.na(days_to_newyear), days_to_newyear := 99]
    data[, next_newyear := NULL]
    
    data = data[event_name_1 == 'ValentinesDay', .(next_ValentinesDay = date), key = 'date'][data, on = 'date', roll = -Inf]
    data[, days_to_ValentinesDay := next_ValentinesDay - date]
    data[days_to_ValentinesDay > 14, days_to_ValentinesDay := 99]
    data[is.na(days_to_ValentinesDay), days_to_ValentinesDay := 99]
    data[, next_ValentinesDay := NULL]
    
    data = data[event_name_1 == 'PresidentsDay', .(next_PresidentsDay = date), key = 'date'][data, on = 'date', roll = -Inf]
    data[, days_to_PresidentsDay := next_PresidentsDay - date]
    data[days_to_PresidentsDay > 14, days_to_PresidentsDay := 99]
    data[is.na(days_to_PresidentsDay), days_to_PresidentsDay := 99]
    data[, next_PresidentsDay := NULL]
    data[state_id == 'WI', days_to_PresidentsDay := 99] # WI does not observe Presidents day
    
    data = data[event_name_1 == 'StPatricksDay', .(next_StPatricksDay = date), key = 'date'][data, on = 'date', roll = -Inf]
    data[, days_to_StPatricksDay := next_StPatricksDay - date]
    data[days_to_StPatricksDay > 14, days_to_StPatricksDay := 99]
    data[is.na(days_to_StPatricksDay), days_to_StPatricksDay := 99]
    data[, next_StPatricksDay := NULL]
    
    data = data[event_name_1 == 'Cinco De Mayo', .(next_CincoDeMayo = date), key = 'date'][data, on = 'date', roll = -Inf]
    data[, days_to_CincoDeMayo := next_CincoDeMayo - date]
    data[days_to_CincoDeMayo > 14, days_to_CincoDeMayo := 99]
    data[is.na(days_to_CincoDeMayo), days_to_CincoDeMayo := 99]
    data[, next_CincoDeMayo := NULL]
    
    data = data[event_name_1 == "Mother's day", .(next_MothersDay = date), key = 'date'][data, on = 'date', roll = -Inf]
    data[, days_to_MothersDay := next_MothersDay - date]
    data[days_to_MothersDay > 14, days_to_MothersDay := 99]
    data[is.na(days_to_MothersDay), days_to_MothersDay := 99]
    data[, next_MothersDay := NULL]
    
    data = data[event_name_1 == "Father's day", .(next_FathersDay = date), key = 'date'][data, on = 'date', roll = -Inf]
    data[, days_to_FathersDay := next_FathersDay - date]
    data[days_to_FathersDay > 14, days_to_FathersDay := 99]
    data[is.na(days_to_FathersDay), days_to_FathersDay := 99]
    data[, next_FathersDay := NULL]
    
    data = data[event_name_1 == "NBAFinalsStart", .(next_NBAFinalsStart = date), key = 'date'][data, on = 'date', roll = -Inf]
    data[, days_to_NBAFinalsStart := next_NBAFinalsStart - date]
    data[days_to_NBAFinalsStart > 14, days_to_NBAFinalsStart := 99]
    data[is.na(days_to_NBAFinalsStart), days_to_NBAFinalsStart := 99]
    data[, next_NBAFinalsStart := NULL]
    data[date >= '2011-05-31' & date <= '2011-06-12', days_to_NBAFinalsStart := 0]
    data[date >= '2012-06-12' & date <= '2012-06-21', days_to_NBAFinalsStart := 0]
    data[date >= '2013-06-06' & date <= '2013-06-20', days_to_NBAFinalsStart := 0]
    data[date >= '2014-06-05' & date <= '2014-06-15', days_to_NBAFinalsStart := 0]
    data[date >= '2015-06-04' & date <= '2015-06-16', days_to_NBAFinalsStart := 0]
    data[date >= '2016-06-02' & date <= '2016-06-19', days_to_NBAFinalsStart := 0]
    
    data = data[event_name_1 == "LaborDay", .(next_LaborDay = date), key = 'date'][data, on = 'date', roll = -Inf]
    data[, days_to_LaborDay := next_LaborDay - date]
    data[days_to_LaborDay > 14, days_to_LaborDay := 99]
    data[is.na(days_to_LaborDay), days_to_LaborDay := 99]
    data[, next_LaborDay := NULL]
    
    data = data[event_name_1 == "MartinLutherKingDay", .(next_MartinLutherKingDay = date), key = 'date'][data, on = 'date', roll = -Inf]
    data[, days_to_MartinLutherKingDay := next_MartinLutherKingDay - date]
    data[days_to_MartinLutherKingDay > 14, days_to_MartinLutherKingDay := 99]
    data[is.na(days_to_MartinLutherKingDay), days_to_MartinLutherKingDay := 99]
    data[, next_MartinLutherKingDay := NULL]
    
    data = data[event_name_1 == "MemorialDay", .(next_MemorialDay = date), key = 'date'][data, on = 'date', roll = -Inf]
    data[, days_to_MemorialDay := next_MemorialDay - date]
    data[days_to_MemorialDay > 14, days_to_MemorialDay := 99]
    data[is.na(days_to_MemorialDay), days_to_MemorialDay := 99]
    data[, next_MemorialDay := NULL]

    data[, event_name_1 := as.integer(as.factor(event_name_1))]
    # 
    # # expanding window features
    # 
    # price features
    data[, avg_yearly_price := mean(sell_price, na.rm = TRUE), by = .(item_id, year)]
    data[, yearly_price_ratio := sell_price / avg_yearly_price]
    data[, promotion := fifelse(yearly_price_ratio < 0.8, 1, 0)]
    
    # price momentums
    # sell_prices = data[, .(sell_price = unique(sell_price)), .(item_id, wm_yr_wk, month, year)][order(item_id, wm_yr_wk)]
    # sell_prices = sell_prices[!duplicated(sell_prices, by = c('wm_yr_wk', 'item_id', 'sell_price', 'year'))]
    # 
    # first_seen_price = sell_prices[, .(first_sell_price = head(sell_price, 1)), by = .(item_id)]
    # sell_prices[first_seen_price, hist_price_change := sell_price / first_sell_price, on = .(item_id)]
    # 
    # sell_prices[, lw_sell_price := shift(sell_price), by = .(item_id)]
    # sell_prices[, weekly_price_momentum := sell_price/lw_sell_price, by = .(item_id)]
    # 
    # avg_monthly_sell_prices = sell_prices[, .(sell_price = mean(sell_price)), by = .(item_id, month)]
    # avg_monthly_sell_prices[, lm_sell_price := shift(sell_price), by = .(item_id)]
    # avg_monthly_sell_prices[, monthly_price_momentum := sell_price/lm_sell_price]
    # sell_prices[avg_monthly_sell_prices, `:=` (lm_sell_price = i.lm_sell_price
    #                                            , monthly_price_momentum = i.monthly_price_momentum), on = c('item_id', 'month')]
    # 
    # avg_yearly_sell_prices = sell_prices[, .(sell_price = mean(sell_price)), by = .(item_id, year)]
    # avg_yearly_sell_prices[, ly_sell_price := shift(sell_price), by = .(item_id)]
    # avg_yearly_sell_prices[, yearly_price_momentum := sell_price/ly_sell_price]
    # sell_prices[avg_yearly_sell_prices, `:=` (ly_sell_price = i.ly_sell_price
    #                                           , yearly_price_momentum = i.yearly_price_momentum), on = c('item_id', 'year')]
    # 
    # data[sell_prices, `:=` (hist_price_change = i.hist_price_change
    #                         , lw_sell_price = i.lw_sell_price
    #                         , lm_sell_price = i.lm_sell_price
    #                         , ly_sell_price = i.ly_sell_price
    #                         , weekly_price_momentum = i.weekly_price_momentum
    #                         , monthly_price_momentum = i.monthly_price_momentum
    #                         , yearly_price_momentum = i.yearly_price_momentum
    # ), on = c('item_id', 'wm_yr_wk')]

    # # price summary stats by year
    # data[, `:=` (sell_price_year_max = max(sell_price, na.rm = TRUE)
    #              , sell_price_year_min = min(sell_price, na.rm = TRUE)
    #              , sell_price_year_sd = sd(sell_price, na.rm = TRUE)
    #              , sell_price_year_mean = mean(sell_price, na.rm = TRUE)
    #              , sell_price_year_median = median(sell_price, na.rm = TRUE)), by = .(item_id, year)]
    
    # price summary stats by month
    # data[, `:=` (sell_price_month_max = max(sell_price, na.rm = TRUE)
    #              , sell_price_month_min = min(sell_price, na.rm = TRUE)
    #              , sell_price_month_sd = sd(sell_price, na.rm = TRUE)
    #              , sell_price_month_mean = mean(sell_price, na.rm = TRUE)
    #              , sell_price_month_median = median(sell_price, na.rm = TRUE)), by = .(item_id, month)]
    
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








