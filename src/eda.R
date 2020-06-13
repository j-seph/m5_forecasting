library(data.table)
library(ggplot2)
library(gridExtra)
library(dygraphs)
library(zoo)

## load in data
train = fread('src/input//sales_train_validation.csv')
cal = fread('src/input/calendar.csv')
price = fread('src/input/sell_prices.csv')

## eda - train
train[, uniqueN(id)] # 30490 unique item store comb.

train[, uniqueN(item_id)] # 3049 unique items

train[, uniqueN(dept_id)] # 7 unique departments

train[, uniqueN(cat_id)] # 3 unique categories

train[, uniqueN(store_id)] # 10 unique stores

train[, uniqueN(state_id)] # 3 unique states

# make into long format
train_m = melt.data.table(train, measure.vars = patterns('^d_'), variable.name = 'd', value.name = 'sales', variable.factor = FALSE)

# sku sales
sku_sales = train_m[, .(sales = sum(sales)), by = .(item_id)][order(-sales)]
sku_sales[, rank := .I]

ggplot(sku_sales) +
    geom_line(aes(x = seq_along(item_id), y = cumsum(sales)/sum(sales)))

# store sales 
store_sales = train_m[, .(sales = sum(sales)), by = .(store_id)][order(-sales)]
store_sales[, rank := .I]

# store sku count
store_sku_count = train_m[, .(sku_count = uniqueN(item_id)), by = .(store_id)][order(-sku_count)]
store_sku_count[, rank := .I]

# nstores 
nstores = train_m[, .(nstores = uniqueN(store_id)), by = .(item_id)][order(-nstores)]

# ndates
ndates = train_m[, .(ndates = uniqueN(d)), by = .(item_id)][order(-ndates)]

train_m = cal[train_m, on = 'd']
train_m = price[train_m, on = c('store_id', 'item_id', 'wm_yr_wk')]
train_m[, date := as.IDate(date)]
min_date = train_m[, min(date)]
train_m[, days_since_min := as.integer(difftime(date, min_date, units = 'days')), by = .(date)]
train_m[, week_index := (days_since_min %/% 7) + 1]
train_m[, item_id_ := factor(item_id, levels = rev(sku_sales$item_id))]

# sku sales
sku_day_sales = train_m[, .(sales = sum(sales)), by = .(date, item_id)]
ggplot(sku_day_sales, aes(x = date, y = sales, color = item_id)) +
    geom_line(show.legend = FALSE) +
    theme_minimal()

sku_week_sales = train_m[, .(sales = sum(sales)), by = .(week_index, item_id)]
ggplot(sku_week_sales, aes(x = week_index, y = sales, color = item_id)) +
    geom_line(show.legend = FALSE) +
    theme_minimal()

# department sales
dept_day_sales = train_m[, .(sales = sum(sales)), by = .(date, dept_id)]
ggplot(dept_day_sales, aes(x = date, y = sales, color = dept_id)) +
    geom_line() +
    theme_minimal()

dept_week_sales = train_m[, .(sales = sum(sales)), by = .(week_index, dept_id)]
ggplot(dept_week_sales, aes(x = week_index, y = sales, color = dept_id)) +
    geom_line() +
    theme_minimal()

# category sales
cat_day_sales = train_m[, .(sales = sum(sales)), by = .(date, cat_id)]
ggplot(cat_day_sales, aes(x = date, y = sales, color = cat_id)) +
    geom_line() +
    theme_minimal()

cat_week_sales = train_m[, .(sales = sum(sales)), by = .(week_index, cat_id)]
ggplot(cat_week_sales, aes(x = week_index, y = sales, color = cat_id)) +
    geom_line() +
    theme_minimal()

# sell price range
sell_price_diff = train_m[, .(sp_min = min(sell_price, na.rm = T),
                              sp_max = max(sell_price, na.rm = T),
                              sp_diff = max(sell_price, na.rm = T) - min(sell_price, na.rm = T),
                             sp_pct_diff = ((max(sell_price, na.rm = T) - min(sell_price, na.rm = T))/
                                 ((max(sell_price, na.rm = T) + min(sell_price, na.rm = T)) / 2)) * 100,
                             sp_pct_chng = ((max(sell_price, na.rm = T) - min(sell_price, na.rm = T))/
                                 min(sell_price, na.rm = T)) * 100), by = .(store_id, item_id)]

sell_price_diff = sell_price_diff[order(-sp_pct_diff)]
sell_price_diff[, rank := .I]

