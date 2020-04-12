library(data.table)
library(ggplot2)
library(gridExtra)

## load in data
train = fread('input/sales_train_validation.csv')
cal = fread('input/calendar.csv')

## eda - train
train[, uniqueN(id)] # 30490 unique item store comb.

train[, uniqueN(item_id)] # 3049 unique items

train[, uniqueN(dept_id)] # 7 unique departments

train[, uniqueN(cat_id)] # 3 unique categories

train[, uniqueN(store_id)] # 10 unique stores

train[, uniqueN(state_id)] # 3 unique states

# make into long format
train_long = melt.data.table(train, measure.vars = patterns('^d_'), variable.name = 'd', value.name = 'unit_sales', variable.factor = FALSE)

# avg unit sales by state
wi_data = train_long[state_id == 'WI']
ca_data = train_long[state_id == 'CA']
tx_data = train_long[state_id == 'TX']

p_wi = ggplot(wi_data[, .(avg_unit_sales = mean(unit_sales, na.rm=T)), d], aes(1:1913, avg_unit_sales)) + geom_line()
p_ca = ggplot(ca_data[, .(avg_unit_sales = mean(unit_sales, na.rm=T)), d], aes(1:1913, avg_unit_sales)) + geom_line()
p_tx = ggplot(tx_data[, .(avg_unit_sales = mean(unit_sales, na.rm=T)), d], aes(1:1913, avg_unit_sales)) + geom_line()

grid.arrange(p_wi, p_ca, p_tx, nrow=3)

# avg unit sales by store
store_sales_data = train_long[, .(avg_unit_sales = mean(unit_sales)), .(store_id, d)]
store_sales_data[, day_dt := as.numeric(gsub('^d_', '', d))]

ggplot(store_sales_data, aes(day_dt, avg_unit_sales)) + 
    geom_line() + 
    facet_grid(store_id ~ .)

# avg unit sales by store, cat
ilcd_data = train_long[, .(avg_unit_sales = mean(unit_sales)), .(store_id, cat_id, d)]
ilcd_data[, day_dt := as.numeric(gsub('^d_', '', d))]

ggplot(ilcd_data, aes(day_dt, avg_unit_sales)) + 
    geom_line() + 
    facet_grid(store_id + cat_id ~ .)



