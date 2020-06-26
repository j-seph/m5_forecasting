evaluate_metrics = function(data, config) {
    
    data = copy(data)
    
    data[, day_idnt := as.integer(gsub('d_', '', d))]
    prediction = data[day_idnt >= config$tr_last + 1 & day_idnt <= config$tr_last + 28, .(d, item_id, store_id, sales)]
    prediction[, d := as.character(d)]
    prediction[, preds := sales]
    prediction[, sales := NULL]
    
    evaluation = fread('src/input/sales_train_evaluation.csv')
    evaluation = melt(evaluation,
                      measure.vars = patterns('^d_'),
                      variable.name = 'd',
                      value.name = 'sales')
    evaluation[, d := as.character(d)]
    
    #prediction[, id := sub('validation', 'evaluation', id)]
    
    prediction = evaluation[prediction, on=c('item_id', 'store_id', 'd')]
    rm(evaluation)
    
    rmse = Metrics::rmse(prediction[, sales], prediction[, preds])
    mape = Metrics::mape(prediction[, sales], prediction[, preds])
    
    prediction[, d_ := as.numeric(gsub('d_', '', d))]
    # test = prediction[, .(sales = sum(sales), preds = sum(preds)), by = .(d_)]
    # test = melt(test, 'd_')
    # pred_plot = ggplot(test, aes(x = d_, y = value, color = variable)) +
    #     geom_line()
    
    eval = list(
        metrics = list(
            rmse = rmse
            , mape = mape
        )
        , data = prediction
        # , predictions_plot = pred_plot
    )
    
    return(eval)
    
}