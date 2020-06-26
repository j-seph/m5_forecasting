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
            , nthread = 24
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
    , lightgbm = list(
        params = list(
            objective = "poisson",
            metric ="rmse",
            force_row_wise = TRUE,
            learning_rate = 0.075,
            num_leaves = 128,
            min_data = 100,
            sub_feature = 0.8,
            sub_row = 0.75,
            bagging_freq = 1,
            lambda_l2 = 0.1,
            nthread = 24
        )
        , model = function(dtrain, dtest, params) {
            lgb_model = lgb.train(params = params
                                  , data = dtrain
                                  , nrounds = 4000
                                  , valids = list(train = dtrain, val = dtest)
                                  , early_stopping_rounds = 400
                                  , eval_freq = 400)
        }
    )
)
