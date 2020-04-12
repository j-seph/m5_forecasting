library(data.table)

TARGET = 'sales'
END_TRAIN  = 1913
MAIN_INDEX = c('id', 'd')

train_df = fread('input/sales_train_validation.csv')
prices_df = fread('input/sell_prices.csv')
calendar_df = fread('input/calendar.csv')

index_columns = c('id', 'item_id', 'dept_id', 'cat_id', 'store_id', 'state_id')
grid_df = melt.data.table(train_df, id.vars = index_columns, variable.name = 'd', value.name = TARGET)

# test grid
add_grid = data.table()
for (i in 1:29) {
    temp_df = unique(train_df[, ..index_columns])
    temp_df[, d := paste0('d_', END_TRAIN + i)]
    temp_df[, sales := NA]
    add_grid = rbind(add_grid, temp_df)
}

# combine test grid with train grid
grid_df = rbind(grid_df, add_grid)

rm(temp_df, train_df, add_grid)

## product release date
release_df = prices_df[, .(release_wk = min(wm_yr_wk)), .(store_id, item_id)]
grid_df = release_df[grid_df, on=c('store_id', 'item_id')]

rm(release_df)

grid_df = calendar_df[, .(wm_yr_wk, d)][grid_df, on='d']

grid_df = grid_df[wm_yr_wk > release_wk]

fwrite(grid_df, 'checkpoint/grid_pt1.csv')

## prices
prices_df[, `:=` (price_max = max(sell_price, na.rm=TRUE),
                  price_min = min(sell_price, na.rm=TRUE),
                  price_sd = sd(sell_price, na.rm=TRUE),
                  price_mean = mean(sell_price, na.rm=TRUE)), .(store_id, item_id)]
prices_df[, price_norm := (sell_price)/(price_max)]
prices_df[, price_n := uniqueN(sell_price), .(store_id, item_id)]
prices_df[, item_n := uniqueN(item_id), .(store_id, sell_price)]

## calendar
calendar_prices = unique(copy(calendar_df[, .(wm_yr_wk, month, year)]))
calendar_prices = calendar_prices[!duplicated(wm_yr_wk)]
prices_df = calendar_prices[prices_df, on='wm_yr_wk']

rm(calendar_prices)

prices_df[, price_momentum := sell_price / shift(sell_price), by = .(store_id, item_id)]
prices_df[, price_momentum_month := sell_price / mean(sell_price), by = .(store_id, item_id, month)]
prices_df[, price_momentum_year := sell_price / mean(sell_price), by = .(store_id, item_id, year)]
prices_df[, `:=` (month = NULL, year = NULL)]

