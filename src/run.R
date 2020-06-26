if (interactive()) {
    rm(list = ls()); gc(); graphics.off(); cat('\014')
}

library(data.table)
library(xgboost)
library(lightgbm)
library(fst)
library(caret)
library(Matrix)

source('src/functions/generate_model_code.R')
source('src/functions/dataset.R')
source('src/functions/feature_engineering.R')
source('src/functions/models.R')
source('src/functions/train.R')
source('src/functions/save_feature_importance.R')
source('src/functions/predict.R')
source('src/functions/evaluate.R')

model_codes = c("CA_1_HOBBIES_1", "CA_1_HOBBIES_2", "CA_1_HOUSEHOLD_1", "CA_1_HOUSEHOLD_2", 
                "CA_1_FOODS_1", "CA_1_FOODS_2", "CA_1_FOODS_3", "CA_2_HOBBIES_1", 
                "CA_2_HOBBIES_2", "CA_2_HOUSEHOLD_1", "CA_2_HOUSEHOLD_2", "CA_2_FOODS_1", 
                "CA_2_FOODS_2", "CA_2_FOODS_3", "CA_3_HOBBIES_1", "CA_3_HOBBIES_2", 
                "CA_3_HOUSEHOLD_1", "CA_3_HOUSEHOLD_2", "CA_3_FOODS_1", "CA_3_FOODS_2", 
                "CA_3_FOODS_3", "CA_4_HOBBIES_1", "CA_4_HOUSEHOLD_1", "CA_4_HOUSEHOLD_2", 
                "CA_4_FOODS_1", "CA_4_FOODS_2", "CA_4_FOODS_3", "TX_1_HOBBIES_1", 
                "TX_1_HOBBIES_2", "TX_1_HOUSEHOLD_1", "TX_1_HOUSEHOLD_2", "TX_1_FOODS_1", 
                "TX_1_FOODS_2", "TX_1_FOODS_3", "TX_2_HOBBIES_1", "TX_2_HOBBIES_2", 
                "TX_2_HOUSEHOLD_1", "TX_2_HOUSEHOLD_2", "TX_2_FOODS_1", "TX_2_FOODS_2", 
                "TX_2_FOODS_3", "TX_3_HOBBIES_1", "TX_3_HOBBIES_2", "TX_3_HOUSEHOLD_1", 
                "TX_3_HOUSEHOLD_2", "TX_3_FOODS_1", "TX_3_FOODS_2", "TX_3_FOODS_3", 
                "WI_1_HOBBIES_1", "WI_1_HOBBIES_2", "WI_1_HOUSEHOLD_1", "WI_1_HOUSEHOLD_2", 
                "WI_1_FOODS_1", "WI_1_FOODS_2", "WI_1_FOODS_3", "WI_2_HOBBIES_1", 
                "WI_2_HOBBIES_2", "WI_2_HOUSEHOLD_1", "WI_2_HOUSEHOLD_2", "WI_2_FOODS_1", 
                "WI_2_FOODS_2", "WI_2_FOODS_3", "WI_3_HOBBIES_1", "WI_3_HOBBIES_2", 
                "WI_3_HOUSEHOLD_1", "WI_3_HOUSEHOLD_2", "WI_3_FOODS_1", "WI_3_FOODS_2", 
                "WI_3_FOODS_3", "CA_4_HOBBIES_2")

config = list(
    input_path = 'src/input/'
    , model_version = toupper(stringi::stri_rand_strings(1, 6))
    , h = 28
    , test_start_date = as.IDate('2016-03-28') # 2016-04-25
    , max_lags = 365
    , tr_last = 1885 # 1913
    , model = 'lgb'
    , categorical_feats = c('wday', 'mday', 'week', 'month', 'year', 'quarter', 'yday', 'first_half_month', 'season'
                            , 'event_name_1', 'item_id', 'promotion', 'snap_CA', 'snap_TX', 'snap_WI')
)

for (mc in model_codes) {
    
    config$model_code = mc
    config$output_path = paste0('src/output/', config$model_version, '/')
    config$model_path = paste0(config$output_path, 'models/')
    config$saved_model = paste0(config$model_path, config$model, '_', config$model_code, '.txt')
    config$metadata_path = paste0(config$output_path, 'metadata/')
    config$log_path = paste0(config$output_path, 'logs/')
    config$plot_path = paste0(config$output_path, 'plots/')
    config$predictions_path = paste0(config$output_path, 'predictions/')
    config$submission_path = paste0(config$output_path, 'submission/')
    
    config$add_to_logfile = function(msg, linebreak = FALSE) {
        if (linebreak) {
            msg = paste('\n', Sys.time(), msg)
        } else {
            msg = paste(Sys.time(), msg)
        }
        if (!dir.exists(config$log_path)) {
            dir.create(config$log_path)
            file.create(paste0(config$log_path, 'log.txt'))
        }
        cat(msg, file = paste0(config$log_path, 'log.txt'), append = TRUE, sep = '\n')
        return(base::message(msg))
    }
    
    if (!dir.exists(config$output_path)) dir.create(config$output_path)
    
    config$add_to_logfile(paste(paste0('[', match(mc, model_codes), '/', length(model_codes), ']'), 'Starting loop for model code: ', mc))
    
    # create train data
    config$add_to_logfile('Creating train dataset')
    dt = create_dt(is_train = TRUE, config = config)
    
    # feature engineering
    config$add_to_logfile('Feature engineering')
    dt = feature_eng(dt)
    
    # replace Inf with NA then omit NA
    for (j in 1:ncol(dt)) set(dt, which(is.infinite(dt[[j]])), j, NA)
    dt = na.omit(dt) # experiment with/without
    
    # train the model
    config$add_to_logfile('Training the model')
    train_model(dt, config)
    
    # save feature importance
    save_feature_importance(config)
    
    # create test data
    config$add_to_logfile('Creating test dataset')
    test_dt = create_dt(is_train = FALSE, config)
    
    test_dt = na.omit(test_dt)
    
    # predict
    config$add_to_logfile('Scoring test dataset')
    test_dt = predict_sales(test_dt, config)
    
    # evaluate
    config$add_to_logfile('Saving predictions')
    eval_result = evaluate_metrics(test_dt, config)
    if (!dir.exists(config$predictions_path)) dir.create(config$predictions_path)
    write.fst(eval_result$data, paste0(config$predictions_path, config$model_code, '.fst'))
    # ggsave(paste0('src/plots/prediction_plots/', mc, '_', eval_result$result$rmse, '.png'), eval_result$predictions_plot)
}

# save model version eval metrics
pred_path = paste0(config$output_path, 'predictions/')
pred_files = list.files(pred_path)
preds = rbindlist(lapply(paste0(pred_path, pred_files), read.fst))
eval = data.table(
    model_version = config$model_version
    , window = fifelse(config$test_start_date == '2016-03-28', 1, 2)
    , rmse = Metrics::rmse(preds$sales, preds$preds)
    , smape = Metrics::smape(preds$sales, preds$preds) # disregard
    , mase = Metrics::mase(preds$sales, preds$preds)
    , rmsle = Metrics::rmsle(preds$sales, preds$preds)
    , mae = Metrics::mae(preds$sales, preds$preds) # disregard
)
fwrite(eval, 'src/scores/eval_metrics.csv', append = T)

scores = fread('src/scores/eval_metrics.csv')

# plot overall predictions against actuals
plot_data = preds[, .(sales = sum(sales), preds = sum(preds)), by = .(store_id, dept_id, d_)]
plot_data = melt(plot_data, id.vars = c('store_id', 'dept_id', 'd_'), measure.vars = c('sales', 'preds'))
ggplot(plot_data, aes(x = d_, y = value, color = variable)) + 
    geom_line() +
    facet_wrap(store_id ~ dept_id, scales = 'free')

# save submission
dir.create(config$submission_path)
preds[d_ >= config$tr_last + 1, id := sub('evaluation', 'validation', id)
   ][d_ >= config$tr_last + 1 + config$h, id := sub("validation", "evaluation", id)
     ][, d := paste0("F", 1:28), by = id
       ][, dcast(.SD, id ~ d, value.var = "preds")
         ][, fwrite(.SD, paste0(config$submission_path, config$model_version, '.csv'))]

sub = fread(paste0(config$submission_path, config$model_version, '.csv'))

sub2 = copy(sub)
sub2[, id := sub('validation', 'evaluation', id)]
sub = rbind(sub, sub2)
fwrite(sub, paste0(config$submission_path, config$model_version, '.csv'))


# for evaluating with wrmsse
submission = fread(paste0(config$submission_path, config$model_version, '.csv'))
setcolorder(submission, c('id', paste0('F', 1:28)))
setnames(submission, c('id', paste0('d_', 1914:1941)))
submission = submission[1:(nrow(submission)/2)]
fwrite(submission, paste0(config$submission_path, config$model_version, '_wrmsse.csv'))

# deep dive into feature importance
feat_imps = rbindlist(lapply(list.files(config$metadata_path), function(x) {
    data = fread(paste0(config$metadata_path, x))
    mc = gsub('_feature_importance.csv', '', x)
    data[, mc := mc]
    data[, gain_rank := frank(-Gain), mc]
    data[, cover_rank := frank(-Cover), mc]
    data[, freq_rank := frank(-Frequency), mc]
}))

feat_imps[, .(avg_rank = mean(gain_rank)), by = .(Feature)][order(avg_rank)]
feat_imps[, .(avg_rank = mean(cover_rank)), by = .(Feature)][order(avg_rank)]
feat_imps[, .(avg_rank = mean(freq_rank)), by = .(Feature)][order(avg_rank)]

# CA base 2.145412 more accurate at start of the horizon
# CA lag 1 2.188809 more accurate as horizon increases
# CA lag 1 + season 2.171115 
# CA lag 1 + season + ohe event_name_type 2.188919
# CA lag 1 + season + days since holiday features 2.166567
# CA lag 1 + season + days since holiday features + sales ratio imputing zeros 2.192764
# CA lag 1 + season + days since hols features + sales ratio imputing zeros + sell price features 2.126697 (new benchmark model)

