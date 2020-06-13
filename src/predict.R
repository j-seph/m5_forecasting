predict_sales = function(data, config) {
    
    data = copy(data)
    
    cat('Predicting:')
    
    xgb_model = xgb.load(config$saved_model)
    
    for (day in as.list(seq(config$test_start_date, length.out = 2*config$h, by = 'day'))){
        cat(as.character(day), '\n')
        tst = data[date >= day - config$max_lags & date <= day]
        tst = feature_eng(tst)
        tst = data.matrix(tst[date == day, -c('id', 'sales', 'date', 'd', 'wm_yr_wk')])
        data[date == day, sales := 1.03 * predict(xgb_model, tst)]
    }
    
    return(data)
}
