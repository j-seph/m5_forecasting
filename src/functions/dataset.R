create_dt = function(is_train = TRUE, config) {
    
    if (is_train) {
        dt = fread(paste0(config$input_path, 'sales_train_validation.csv'))
        
        cols = dt[, names(.SD), .SDcols = patterns('^d_')]
        dt[, (cols) := transpose(lapply(transpose(.SD), 
                                        function(x) {
                                            i = min(which(x > 0))
                                            x[1:i-1] = NA
                                            x})), .SDcols = cols]
    } else {
        dt = fread(paste0(config$input_path, 'sales_train_validation.csv'),
                   drop = paste0('d_', 1:(config$tr_last - config$max_lags)))
        dt[, paste0('d_', (config$tr_last+1):(config$tr_last+config$h)) := 0]
    }
    
    dt = generate_model_code(dt)
    dt = dt[model_code == config$model_code]
    dt[, model_code := NULL]
    
    dt = na.omit(melt(dt,
                      measure.vars = patterns('^d_'),
                      variable.name = 'd',
                      value.name = 'sales'))
    
    cal = fread(paste0(config$input_path, 'calendar.csv'))
    dt = dt[cal, `:=` (date = as.IDate(i.date, format = '%Y-%m-%d'),
                       wm_yr_wk = i.wm_yr_wk,
                       event_name_1 = i.event_name_1,
                       snap_CA = i.snap_CA,
                       snap_TX = i.snap_TX,
                       snap_WI = i.snap_WI), on = 'd']
    
    prices = fread(paste0(config$input_path, 'sell_prices.csv'))
    dt[prices, sell_price := i.sell_price, on = c('store_id', 'item_id', 'wm_yr_wk')]
    
    dt[, sales := as.numeric(sales)]
    
    return(dt)
}
