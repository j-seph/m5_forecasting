save_feature_importance = function(config) {
    
    if (!dir.exists(config$metadata_path)) dir.create(config$metadata_path)
    if (config[['model']] == 'xgb') {
        xgb_model = xgb.load(config$saved_model)
        mat = xgb.importance(feature_names = colnames(dt[, -c('id', 'sales', 'date', 'd', 'wm_yr_wk')]), model = xgb_model)
        fwrite(mat, paste0(config$metadata_path, mc, '_feature_importance.csv'))
    } else if (config[['model']] == 'lgb') {
        lgb_model = lgb.load(config$saved_model)
        mat = lgb.importance(lgb_model)
        fwrite(mat, paste0(config$metadata_path, mc, '_feature_importance.csv'))
    }
}