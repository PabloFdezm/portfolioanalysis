#install.packages('tidyverse')
#install.packages('tidyquant')
#install.packages("crypto2")
#install.packages('tidyr')
#install.packages('xts')
#install.packages('quantmod')
#install.packages('tsbox')


##### Libraries
library(crypto2)
library(tidyverse)
library(tidyquant)
library(tidyr)
library(xts)
library(quantmod)

##### Fintual
fintual_symbols = c('CNRG', 'CXSE', 'ESGV', 'ESPO', 'FTEC', 'KOMP', 'QQQ', 'SMH', 'VT')
fintual_ponds = c(9.27, 1.74,20.13,8.57,6.86,10.34,3.95,17.3,14.71)/100

##### Code -- ETFs
fintual_etfs = tq_get(fintual_symbols,
                      from = '2018-02-01',
                      to = '2021-10-31')

etfs_monthly = fintual_etfs %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly",
               col_rename = "Ra") %>%
  ungroup()

##### Code -- Fintual
# Recordatorio pasar de csv a rds risky_mf

risky_mf = read_rds('risky_mf.rds')
#risky_mf = read.csv('risky_mf.csv')
risky_mf = risky_mf %>%
  mutate(date = as.Date(date), price = as.numeric(price)) %>%
  dplyr::filter(date >= '2018-02-01' & date <= '2021-10-31')


risky_xts = xts(risky_mf$price, order.by = risky_mf$date)

risky_monthly = monthlyReturn(risky_xts)

etfs_monthly = etfs_monthly %>%
  pivot_wider(names_from = 'symbol',values_from = 'Ra')

etfs_monthly = etfs_monthly %>%
  arrange(date) %>%
  dplyr::filter(date >= '2018-10-31')

##### Fixing dates

etfs_dates = etfs_monthly %>% select(date) %>% .[[1]]

etfs_dates1 = append(rep(NA, 8), etfs_dates )

risky_dates = ggplot2::fortify(risky_monthly) %>%
  select(Index) %>% .[[1]]

fix_dates = data.frame(etfs_dates1, risky_dates) %>% 
  mutate(etfs_dates1 = as.Date(etfs_dates1)) %>% 
  mutate(error = etfs_dates1 - risky_dates) %>%
  select(-risky_dates) %>%
  drop_na()

etfs_monthly <- merge(etfs_monthly,fix_dates, by.x = 'date', by.y = 'etfs_dates1') %>%
  mutate(date = date - error) %>%
  select(-error)

##### Merging data bases

risky_monthly = ggplot2::fortify(risky_monthly)

returns_all = merge(risky_monthly, etfs_monthly, by.x = 'Index', by.y = 'date')
 
write_rds(returns_all, 'returns.rds')
