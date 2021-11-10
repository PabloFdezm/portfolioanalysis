#install.packages('tidyverse')
#install.packages('tidyquant')
#install.packages("crypto2")
#install.packages('tidyr')
#install.packages('xts')
#install.packages('quantmod')

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
fintual_etfs = tq_get(fintual_symbols)

etfs_monthly = fintual_etfs %>%
  group_by(symbol) %>%
  tq_transmute(select     = adjusted, 
               mutate_fun = periodReturn, 
               period     = "monthly",
               col_rename = "Ra")

##### Code -- Fintual
# Recordatorio pasar de csv a rds risky_mf

risky_mf = read.csv('risky_mf.csv')
risky_mf = risky_mf %>%
  mutate(date = as.Date(date), price = as.numeric(price))

risky_xts = xts(risky_mf$price, order.by = risky_mf$date)

risky_monthly = monthlyReturn(risky_xts)
