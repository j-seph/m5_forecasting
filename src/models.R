models = list(
    xgboost = list(
        params = list(
            objective = 'count:poisson'
            , eval_metric = 'rmse'
            , eta = 0.075
            , max_depth = 6
            , gamma = 0
            , min_child_weight = 5
            , colsample_bytree = 0.8
            , subsample = 0.75
            , lambda = 0.1
            , max_leaves = 128
        )
        , model = function(dtrain, dtest, params) {
            xgb_model = xgb.train(params = params
                                  , data = dtrain
                                  , nrounds = 4000
                                  , watchlist = list(train = dtrain, val = dtest)
                                  , early_stopping_rounds = 50
                                  , print_every_n = 50
                                  , maximize = FALSE
                                  , verbose = 1)
            
            return(xgb_model)
        }
    )
)
