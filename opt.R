#install.packages('openxlsx')

library(tidyverse)
library(openxlsx)

returns <- read_rds('returns.rds')

#openxlsx::write.xlsx(returns, 'returns.xlsx')

y <- returns[,2]
x <- returns[,3:11]

y[1]
y_hat = rowSums(x * b)

m <- as.data.frame(matrix(c(y,y_hat),ncol=3))
colnames(m) <- c('date', 'y', 'y_hat')


m = m %>% mutate(date = as.Date(date), y = as.numeric(y), y_hat = as.numeric(y_hat)) %>% 
  mutate(error = y - y_hat) %>%
  mutate(error_sqt = round(error^2,4))


##### https://palomar.home.ece.ust.hk/MAFS6010R_lectures/Rsession_solvers.html

# squared l2-norm error of a linear fitting y ~ par[1] + par[2]*x

f <- function(param, x,y) sum((param[1] + param[2]*x[1,1] + param[3]*x[1,2] + param[4]*x[1,3] + param[5]*x[1,4] + param[6]*x[1,5] + param[7]*x[1,6] + param[8]*x[1,7] + param[9]*x[1,8] + param[10]*x[1,9] - y[1])^2)

# call solver (with initial value c(0, 1) and default method = "Nelder-Mead")
result <- optim(par = c(rep(sample(c(0,1),1),10)), f, x=x, y=y, lower = c(0,10), upper = c(1,10), method="L-BFGS-B")
str(result)
#> List of 5
#>  $ par        : num [1:2] -1.27 2.03
#>  $ value      : num 2.82
#>  $ counts     : Named int [1:2] 89 NA
#>   ..- attr(*, "names")= chr [1:2] "function" "gradient"
#>  $ convergence: int 0
#>  $ message    : NULL

# plot linear regression
plot(y ~ x, data = dat, main = "Least square regression")
abline(a = result$par[1], b = result$par[2], col = "red")