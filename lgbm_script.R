## Credit to kxx
library(data.table)
library(lightgbm)

set.seed(0)

h <- 28 
max_lags <- 366
tr_last <- 1913
fday <- as.IDate("2016-04-25") 

#---------------------------
cat("Creating auxiliary functions...\n")

free <- function() invisible(gc())

create_dt <- function(is_train = TRUE, nrows = Inf) {
  
  prices <- fread("input/sell_prices.csv")
  cal <- fread("input/calendar.csv")
  cal[, `:=` (date = as.IDate(date, format="%Y-%m-%d")
              ,is_weekend = as.integer(weekday %chin% c('Saturday', 'Sunday')))]
  
  if (is_train) {
    dt <- fread("input/sales_train_validation.csv", nrows = nrows)
  } else {
    dt <- fread("input/sales_train_validation.csv", nrows = nrows,
                drop = paste0("d_", 1:(tr_last-max_lags)))
    dt[, paste0("d_", (tr_last+1):(tr_last+2*h)) := NA_real_]
  }
  
  dt <- melt(dt,
             measure.vars = patterns("^d_"),
             variable.name = "d",
             value.name = "sales")
  
  dt <- dt[cal, `:=`(date = i.date, 
                     is_weekend = i.is_weekend,
                     wm_yr_wk = i.wm_yr_wk,
                     event_name_1 = i.event_name_1,
                     snap_CA = i.snap_CA,
                     snap_TX = i.snap_TX,
                     snap_WI = i.snap_WI), on = "d"]
  
  dt[prices, sell_price := i.sell_price, on = c("store_id", "item_id", "wm_yr_wk")]
}

create_fea <- function(dt) {
  
  lag <- c(7, 28, 29)
  dt[, (paste0("lag_", lag)) := shift(sales, lag), by = "id"]
  
  win <- c(7, 30, 90, 180)
  dt[, (paste0("roll_mean_28_", win)) := frollmean(lag_28, win, na.rm = T), by = "id"]
  
  win <- c(30)
  dt[, (paste0("roll_max_28_", win)) := frollapply(lag_28, win, max), by = "id"]
  dt[, (paste0("roll_var_28_", win)) := frollapply(lag_28, win, var), by = "id"]
  
  dt[, price_change_1 := sell_price / shift(sell_price) - 1, by = "id"]
  dt[, price_change_365 := sell_price / frollapply(shift(sell_price), 365, max) - 1, by = "id"]
  
  cat_cols <- c("item_id", "state_id", "dept_id", "cat_id", "event_name_1", "store_id")
  dt[, (cat_cols) := lapply(.SD, function(x) as.integer(factor(x))), .SDcols = cat_cols]
  
  dt[, `:=`(wday = wday(date),
            mday = mday(date),
            week = week(date),
            month = month(date),
            quarter = quarter(date),
            year = year(date),
            #store_id = NULL,
            d = NULL,
            wm_yr_wk = NULL)]
}

cat("Creating training set with features...\n")

tr <- create_dt()
free()

create_fea(tr)
free()

tr <- na.omit(tr)
y <- tr$sales

idx <- tr[date <= max(date)-h, which = TRUE] 

tr[, c("id", "sales", "date") := NULL]
free()

tr <- data.matrix(tr)
free()

cat("Constructing training and validation sets for GBM...\n")

cats <- c("item_id", "dept_id", "cat_id", "state_id", "event_name_1", "store_id",
          "snap_CA", "snap_TX", "snap_WI",
          "wday", "mday", "week", "month", "year", "is_weekend")

xtr <- lgb.Dataset(tr[idx, ], label = y[idx], categorical_feature = cats)
xval <- lgb.Dataset(tr[-idx, ], label = y[-idx], categorical_feature = cats)

rm(tr, y, idx)
free()

cat("Training model...\n")

p <- list(objective = "poisson",
          metric ="rmse",
          learning_rate = 0.075,
          sub_feature = 0.8,
          sub_row = 0.75,
          #bagging_fraction = 0.7,
          bagging_freq = 1,
          lambda_l2 = 0.1,
          #alpha = 0.1,
          nthread = -1)

m_lgb <- lgb.train(params = p,
                   data = xtr,
                   nrounds = 2000,
                   valids = list(valid = xval),
                   early_stopping_rounds = 400,
                   eval_freq = 200)

lgb.save(m_lgb, 'model.txt')

cat("Best score:", m_lgb$best_score, "at", m_lgb$best_iter, "iteration") 
lgb.plot.importance(lgb.importance(m_lgb), 20)

rm(xtr, xval, p)
free()

#---------------------------
cat("Forecasting...\n")

# suppressPackageStartupMessages(library(foreach))
# suppressPackageStartupMessages(library(doSNOW))
# 
# set_progress_bar = function(ntasks) {
#   progress = function(n) setTxtProgressBar(txtProgressBar(min=0, max=ntasks, style=3), n)
#   opts = list(progress=progress)
#   return(opts)
# }

te <- create_dt(FALSE)

# cluster = makeCluster(parallel::detectCores() - 1, type = 'SOCK')
# registerDoSNOW(cluster)
# 
# ntasks = length(as.list(seq(fday, length.out = 2*h, by = "day")))
# 
# free()
# 
# system.time({
#   sales = foreach(i = 1:ntasks, .combine = rbind, .options.snow = set_progress_bar(ntasks)) %dopar% {
#     day = as.list(seq(fday, length.out = 2*h, by = "day"))[[i]]
#     tst <- te[date >= day - max_lags & date <= day]
#     create_fea(tst)
#     tst <- data.matrix(tst[date == day][, c("id", "sales", "date") := NULL])
#     te[date == day, predict(m_lgb, tst)]
#   }
# })
# 
# stopCluster(cluster)

system.time({
  for (day in as.list(seq(fday, length.out = 2*h, by = "day"))){
    cat(as.character(day), " ")
    tst <- te[date >= day - max_lags & date <= day]
    create_fea(tst)
    tst <- data.matrix(tst[date == day][, c("id", "sales", "date") := NULL])
    te[date == day, sales := predict(m_lgb, tst)]
  }
})

te[date >= fday
   ][date >= fday+h, id := sub("validation", "evaluation", id)
     ][, d := paste0("F", 1:28), by = id
       ][, dcast(.SD, id ~ d, value.var = "sales")
         ][, fwrite(.SD, "sub_dt_lgb.csv")]

#---------------------------
# library(foreach)
# library(doMC)
# registerDoMC(detectCores() - 1)
# 
# te <- create_dt(FALSE)
# 
# dates = seq(fday, length.out = 2*h, by = "day")
# 
# foreach(i = 1:length(dates), .combine = rbind) %dopar% {
#   tst <- te[date >= dates[i] - max_lags & date <= dates[i]]
#   create_fea(tst)
#   tst <- data.matrix(tst[date == dates[i]][, c("id", "sales", "date") := NULL])
#   te[date == dates[i], sales := predict(m_lgb, tst)]
# }
# 
# for (day in as.list(seq(fday, length.out = 2*h, by = "day"))){
#   cat(as.character(day), " ")
#   tst <- te[date >= day - max_lags & date <= day]
#   create_fea(tst)
#   tst <- data.matrix(tst[date == day][, c("id", "sales", "date") := NULL])
#   te[date == day, sales := predict(m_lgb, tst)]
# }
# 
# te[date >= fday
#    ][date >= fday+h, id := sub("validation", "evaluation", id)
#      ][, d := paste0("F", 1:28), by = id
#        ][, dcast(.SD, id ~ d, value.var = "sales")
#          ][, fwrite(.SD, "sub_dt_lgb.csv")]
