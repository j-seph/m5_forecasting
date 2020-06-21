
train_model = function(data, config, na_omit = TRUE) {
    
    if (na_omit) {
        idx = data[date <= max(date) - config$h, which = TRUE]
        
        labels = data$sales
        
        train_mat = data.matrix(data[idx, -c('id', 'date', 'sales', 'd', 'wm_yr_wk')])
        
        train_label = labels[idx]
        
        dtrain = xgb.DMatrix(data = train_mat, label = train_label)
        
        valid_mat = data.matrix(data[-idx, -c('id', 'date', 'sales', 'd', 'wm_yr_wk')])
        
        valid_label = labels[-idx]
        
        dvalid = xgb.DMatrix(data = valid_mat, label = valid_label)
        
        rm(data, train_mat, valid_mat)
        
    } else {
        train = data[date <= config$train_end_date, config$features, with = FALSE]
        
        train_mat = data.matrix(train[, -c('id', 'date', 'sales')])
        
        train_label = train$sales
        
        dtrain = xgb.DMatrix(data = train_mat, label = train$sales)
        
        valid = data[date > config$train_end_date & date <= config$train_end_date + config$h, config$features, with = FALSE]
        
        valid_mat = data.matrix(valid[, -c('id', 'date', 'sales')])
        
        valid_label = valid$sales
        
        dvalid = xgb.DMatrix(data = valid_mat, label = valid$sales)
        
        rm(data, train, train_mat, valid, valid_mat)
    }
    
    
    
    # cv_res = xgb.cv(params = models$xgboost$params,
    #                 data = dtrain,
    #                 nrounds = 1000,
    #                 nfold = 5,
    #                 showsd = TRUE,
    #                 metrics = 'rmse',
    #                 print_every_n = 10,
    #                 early_stopping_rounds = 20)
    
    xgb_model = models$xgboost$model(dtrain = dtrain, dtest = dvalid, params = models$xgboost$params)
    
    dir.create(config$model_path)
    xgb.save(xgb_model, config$saved_model)
}









# 
# plot(test_eval$actual, test_eval$pred)
# 
# 
# 
# mat <- xgb.importance(feature_names = colnames(train), model = xgb_model)
# xgb.plot.importance(importance_matrix = mat)
# 
# rm(data, train, dtrain, dvalid, valid, valid_mat); invisible(gc())
# 
# # hyperparameter tuning
# grid = expand.grid(
#     nrounds = 561,
#     eta = 0.2,
#     max_depth = 6,
#     gamma = 0,
#     min_child_weight = 5,
#     colsample_bytree = 1,
#     subsample = 1
# )
# 
# xgb_tc = trainControl(
#     method = "cv",
#     number = 5,
#     verboseIter = TRUE,
#     returnData = FALSE,
#     returnResamp = "all",
#     allowParallel = TRUE
# )
# 
# 
# xgb_tune = train(
#     x = train_mat,
#     y = train_label,
#     method="xgbTree",
#     trControl=xgb_tc,
#     tuneGrid=grid,
#     verbose=T,
#     metric="RMSE",
#     nthread = 24
# )
# 
# 




# wrmsse_custom = function(preds, dtrain) {
#     
#     # actual observed values
#     labels = getinfo(dtrain, 'label')
#     
#     # number of columns
#     num_col = length(labels) %/% NUM_ITEMS
#     
#     # reshape data to original
#     reshaped_preds = (matrix(preds, nrow = NUM_ITEMS))
#     reshaped_true = (matrix(labels, nrow = NUM_ITEMS))
#     
#     train = as.matrix(weight %*% cbind(reshaped_preds, reshaped_true))
#     
#     score = sum(sqrt(apply((train[, 1:num_col] - train[, (num_col + 1):(2 * num_col)]) ^ 2, 1, mean) / weight1) * weight2)
#     
#     return(list(
#         name = 'wrmsse'
#         , value = score
#         , higher_better = FALSE
#     ))
# }