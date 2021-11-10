# risky_mf <- read.csv("C:/Users/pablo/Downloads/k3uw2pz4.csv")

library(dplyr)

colnames(risky_mf) <- c('date','price')

risky_mf = risky_mf %>%
  mutate(date = as.Date(date), price = as.numeric(price))

write.csv(risky_mf, 'risky_mf.csv')
