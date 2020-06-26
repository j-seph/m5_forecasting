get_weight_df = function(calendar, prices, train_df, weight_columns, id_columns) {
    day_to_week = unique(calendar[, .(d, wm_yr_wk)])
    weight_df = train_df[, c('item_id', 'store_id', weight_columns), with = FALSE]
    weight_df = melt.data.table(weight_df, id.vars = c('item_id', 'store_id'), variable.name = 'd', value.name = 'value')
    weight_df = day_to_week[weight_df, on = 'd']
    weight_df = prices[weight_df, on = c('item_id', 'store_id', 'wm_yr_wk')]
    weight_df[, value := value * sell_price]
    weight_df = dcast.data.table(weight_df, item_id + store_id ~ d, value.var = 'value')
    weight_df = train_df[, id_columns, with = FALSE][weight_df, on = c('item_id', 'store_id')]
    return(weight_df)
}

rmsse = function(valid_preds, lv, meta_data, group_id) {
    valid_y = meta_data[['val']][[lv]][, -group_id, with = FALSE]
    score = ((valid_y - valid_preds) ** 2)[, rowMeans(.SD)]
    scale = unlist(meta_data[['scales']][[lv]])
    return(sqrt(score/scale))
}

score = function(valid_preds, valid_df, meta_data) {
    
    id_columns = c('id', 'item_id', 'dept_id', 'cat_id', 'store_id', 'state_id', 'all_id')
    valid_target_columns = names(valid_preds)
    valid_preds = cbind(valid_df[, id_columns, with = FALSE], valid_preds)
    
    group_ids = list(
        'all_id', 'cat_id', 'state_id', 'dept_id', 'store_id', 'item_id'
        , c('state_id', 'cat_id')
        , c('state_id', 'dept_id')
        , c('store_id', 'cat_id')
        , c('store_id', 'dept_id')
        , c('item_id', 'state_id')
        , c('item_id', 'store_id')
    )
    
    grp_ids = c()
    all_scores = c()
    
    for (i in 1:length(group_ids)) {
        print(i)
        lv_scores = rmsse(valid_preds[, lapply(.SD, sum), by = c(group_ids[[i]]), .SDcols = valid_target_columns][order(mget(group_ids[[i]]))][, -group_ids[[i]], with = FALSE], i, meta_data, group_ids[[i]])
        weight = meta_data[['weights']][[i]][, -group_ids[[i]], with = FALSE]
        lv_scores = cbind(weight, lv_scores)
        lv_scores[, scores := weight * lv_scores]
        grp_ids = append(grp_ids, group_ids[[i]])
        all_scores = append(all_scores, sum(lv_scores$scores))
    }
    
    res = list(group_ids = grp_ids
               , all_scores = all_scores)
    
    return (res)
    
}


wrmsse_evaluator = function(train_df, valid_df, calendar, prices) {
    
    train_df = copy(train_df)
    valid_df = copy(valid_df)
    calendar = copy(calendar)
    prices = copy(prices)
    
    train_y = train_df[, names(train_df)[grepl('^d_', names(train_df))], with = FALSE]
    train_target_columns = names(train_y)
    weight_columns = tail(train_target_columns, 28)
    
    train_df[, all_id := 0]
    
    id_columns = names(train_df[, -c(train_target_columns), with = FALSE])
    valid_target_columns = names(valid_df[, names(valid_df)[grepl('^d_', names(valid_df))], with = FALSE])
    
    if (!all(id_columns %in% valid_df)) {
        valid_df = cbind(train_df[, id_columns, with = FALSE], valid_df)
    }
    
    weight_df = get_weight_df(calendar, prices, train_df, weight_columns, id_columns)
    
    group_ids = list(
        'all_id', 'cat_id', 'state_id', 'dept_id', 'store_id', 'item_id'
        , c('state_id', 'cat_id')
        , c('state_id', 'dept_id')
        , c('store_id', 'cat_id')
        , c('store_id', 'dept_id')
        , c('item_id', 'state_id')
        , c('item_id', 'store_id')
    )
    
    scales = lapply(1:length(group_ids), function(x) {
        message(paste0('group_id:', x))
        train_y = train_df[, lapply(.SD, sum), by = c(group_ids[[x]]), .SDcols = train_target_columns][order(mget(group_ids[[x]]))]
        scale = lapply(1:nrow(train_y), function(r) {
            message(paste0('row:',r))
            series = as.vector(as.matrix(train_y[r, -group_ids[[x]], with = F][, which.max(train_y[r][, -group_ids[[x]], with = FALSE] != 0):ncol(train_y[r, -group_ids[[x]], with = F])]))
            mean((series[2:length(series)] - series[1:(length(series)-1)]) ** 2)
        })
        scale
    })
    
    tr = lapply(1:length(group_ids), function(x) {
        train_y = train_df[, lapply(.SD, sum), by = c(group_ids[[x]]), .SDcols = train_target_columns][order(mget(group_ids[[x]]))]
        train_y
    })
    
    val = lapply(1:length(group_ids), function(x) {
        valid_df[, lapply(.SD, sum), by = c(group_ids[[x]]), .SDcols = valid_target_columns][order(mget(group_ids[[x]]))]
    })
    
    weights = lapply(1:length(group_ids), function(x) {
        lv_weight = weight_df[, lapply(.SD, sum), by = c(group_ids[[x]]), .SDcols = weight_columns][order(mget(group_ids[[x]]))][, rowSums(.SD), by=c(group_ids[[x]])]
        lv_weight[, V1 := V1/sum(V1)]
        lv_weight
    })
    
    return(list(scales = scales
                , tr = tr
                , val = val
                , weights = weights))
}

train_full = fread('src/input/sales_train_evaluation.csv')
valid_df = train_full[, tail(names(train_full), 28), with = FALSE]
train_df = train_full[, -names(valid_df), with = FALSE]
calendar = fread('src/input/calendar.csv')
prices = fread('src/input/sell_prices.csv')

res = wrmsse_evaluator(train_df, valid_df, calendar, prices)
saveRDS(res, 'src/wrmsse_metadata.rds')

test_preds = fread('src/output/RF1RY1/submission/RF1RY1_wrmsse.csv')
sub = fread('src/input/sample_submission.csv')

test_preds = test_preds[sub[, .(id)], on = 'id']
test_preds = test_preds[grepl('validation', id)]
valid_df = cbind(train_df[, .(id, item_id, dept_id, cat_id, store_id, state_id, all_id = 0)], valid_df)
scr = score(test_preds[, -c('id')], valid_df, res)
