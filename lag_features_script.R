library(data.table)

TARGET = 'sales'
END_TRAIN = 1913
MAIN_INDEX = c('id', 'd')

train_df = fread('input/sales_train_validation.csv')

index_columns = c('id', 'item_id', 'dept_id', 'cat_id', 'store_id', 'state_id')
train_df = melt.data.table(train_df, id.vars = index_columns, variable.name = 'd', value.name = TARGET)

## lags creation
temp_df = train_df[, .(id, d, sales)]

system.time({
    for (i in 1:7) {
        print(paste('Shifting:', i))
        temp_df[, paste0('lag_', i) := shift(sales, 1), by = id]
    }
})


## rolling lags
temp_df = train_df[, .(id, d, sales)]

system.time({
    for (i in c(14, 30, 60)) {
        print(paste('Rolling period:', i))
        temp_df[, paste0('rolling_mean_', i) := frollmean(shift(sales, 1), i), by = id]
        temp_df[, paste0('rolling_std_', i) := frollapply(shift(sales, 1), i, sd), by = id]
    }
})

temp_matrix = as.matrix(temp_df[, -c('id', 'd', 'sales')])
temp_matrix = Matrix::Matrix(temp_matrix, sparse = TRUE)

## apply on grid_df
grid_df = fread('checkpoint/grid_pt1.csv')

grid_df = grid_df[, .(id, d, sales)]
SHIFT_DAY = 28

# create lags
system.time({
    LAG_DAYS = SHIFT_DAY:(SHIFT_DAY + 6)
    
    for (i in LAG_DAYS) {
        grid_df[, paste0('sales_lag_', i) := shift(sales, i), by = id]
    }
})

# rolling with 28 day shift
system.time({
    for (i in c(14, 30, 60, 180)) {
        print(paste('Rolling period:', i))
        grid_df[, paste0('rolling_max_', i) := frollapply(shift(sales, SHIFT_DAY), i, max, na.rm=TRUE)]
        grid_df[, paste0('rolling_mean_', i) := frollapply(shift(sales, SHIFT_DAY), i, mean, na.rm=TRUE)]
        grid_df[, paste0('rolling_std_', i) := frollapply(shift(sales, SHIFT_DAY), i, sd, na.rm=TRUE)]
    }
})

fwrite(grid_df, paste0('checkpoint/lags_df_', SHIFT_DAY, '.csv'))






