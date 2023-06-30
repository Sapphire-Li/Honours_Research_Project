library(readxl)
library(fpp3)
library(tidyverse)
library(dplyr)

# https://www.abs.gov.au/statistics/labour/EMPL-and-unEMPL/labour-force-australia-detailed/latest-release
Quarter <- read_xlsx("6291004.xlsx", range = "Data1!A11:A164", col_names = "Quarter")
Total <- read_xlsx("6291004.xlsx", range = "Data1!BI11:BI164", col_names = "Total")

EMPL <- cbind(Quarter, Total)
# 1978 Feb - 2023 Feb

EMPL <- EMPL |> 
  mutate(Quarter = yearquarter(Quarter)) |> 
  as_tsibble(index = "Quarter")
EMPL <- EMPL |> mutate(Log = log(Total))

T <- nrow(EMPL)
# In-sample - Training Set
train <- EMPL[1:floor(nrow(EMPL)*.75),] |> as_tsibble()
R <- nrow(train)
# Out-of-sample - Test set
test <- EMPL[(R+1):nrow(EMPL),] |> as_tsibble()
P <- nrow(test)
m <- 4

# Preliminary Analysis
# train |> autoplot(Total)
# train |> autoplot(log(Total))
# train |> features(Total, features = guerrero)
# train |> gg_season(Total)
# train |> gg_subseries(Total)
# train |> autoplot(difference(log(Total),m))
# train |> gg_tsdisplay(difference(log(Total),m), "partial")
# train |> gg_tsdisplay(difference(difference(log(Total),m),1), "partial")




# Model: ARIMA(2,0,2)(0,1,1)[4] w/ drift 
fit_1 <- train |> 
  model(ARIMA(log(Total)))
report(fit_1)

# Coefficients:
#    ar1      ar2      ma1     ma2     sma1  constant
# 1.7360  -0.7761  -0.7285  0.2714  -0.8716     8e-04
# sigma^2 estimated as 3.451e-05

fit_1_coef <- coef(fit_1) |> 
  select(term, estimate)

# fit_1_coef[1,2] ar1 (phi_1)
# fit_1_coef[2,2] ar2 (phi_2)
# fit_1_coef[3,2] ma1 (theta_1)
# fit_1_coef[4,2] ma2 (theta_2)
# fit_1_coef[5,2] sma1 (Theta_1)
# fit_1_coef[6,2] constant



# log(y_t) = c+\phi_1 log(y_{t-1})+\phi_2 log(y_{t-2}) + log(y_{t-4}) -\phi_1 log(y_{t-5}) -\phi_2 log(y_{t-6}) + \epsilon_t + (\theta_1+ \Theta_1) \epsilon_{t-1} + (\theta_2+\theta_1\Theta_1) \epsilon_{t-2} +\theta_2\Theta_1 \epsilon_{t-3} 

resid_1 <- numeric(P+3) |> as.numeric()

resid_1[1] <- residuals(fit_1) |>
  as_tibble() |>
  select(.resid) |> 
  slice(R-2)

resid_1[2] <- residuals(fit_1) |>
  as_tibble() |>
  select(.resid) |> 
  slice(R-1)

resid_1[3] <- residuals(fit_1) |>
  as_tibble() |>
  select(.resid) |> 
  slice(R)




# Conditional Mean for Total EMPL
mean_1 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  mean_1[j] <- (level[[R+j-1]] + trend[[R+j-1]])*season[[R+j]]
}

# Conditional Variance for Total EMPL
var <- glance(fit_1) |> select(sigma2) |> as.numeric()
var_1 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  var_1[j] <- mean_1[j]^2*var
}
sd_1 <- as.numeric(var_1) |> sqrt()


pd_1 <- numeric(P) |> as.numeric()
for (j in 1:P) {
  pd_1[j] <- dnorm(test[[j,2]], mean_1[j], sd_1[j])
}


LS_1 <- sum(log(pd_1)) / P
LS_1










# Model: ARIMA(1,1,0)(0,1,0)[4] 

fit_2 <- train |> 
  model(ARIMA(log(Total) ~ pdq(0,1,0) + PDQ(1,1,0)))
report(fit_2)

fit_2_coef <- coef(fit_2) |> 
  select(term, estimate)

resid_2 <- residuals(fit_2) |>
  as_tibble() |>
  select(.resid)




# Model: ARIMA(0,1,0)(0,1,0)[4] 

fit_2 <- train |> 
  model(ARIMA(log(Total) ~ pdq(0,1,0) + PDQ(1,1,0)))
report(fit_2)
gg_tsresiduals(fit_2, lag = 16)

fit_2_coef <- coef(fit_2) |> 
  select(term, estimate)

resid_2 <- residuals(fit_2) |>
  as_tibble() |>
  select(.resid)






# Model: ARIMA(0,1,0)(0,1,2)[4] 

fit_3 <- train |> 
  model(ARIMA(log(Total) ~ pdq(0,1,0) + PDQ(0,1,2)))
report(fit_3)
gg_tsresiduals(fit_3, lag = 16)

fit_3_coef <- coef(fit_3) |> 
  select(term, estimate)

resid_3 <- residuals(fit_3) |>
  as_tibble() |>
  select(.resid)






# Model: ARIMA(1,0,4)(0,1,0)[4] 

fit_1 <- train |> 
  model(ARIMA(log(Total)~0))
report(fit_1)

# Coefficients:
#   ar1     ma1    ma2     ma3      ma4
# 0.9518  0.2731  0.359  0.2485  -0.6118
# sigma^2 estimated as 3.894e-05








# Model: LM w/ ARIMA(1,0,0)(2,0,0)[4] errors 

fit_4 <- train |> 
  model(ARIMA(log(Total) ~ trend()))
report(fit_4)
gg_tsresiduals(fit_4, lag = 16)

# Coefficients:
#   ar1    sar1    sar2  trend()  intercept
# 0.9060  0.5298  0.1547   0.0049     8.7886
# sigma^2 estimated as 4.443e-05






# Model: ETS(A,Ad,A) 

fit_5 <- train |> 
  model(ETS(Total))
report(fit_5)
gg_tsresiduals(fit_4, lag = 16)

# Smoothing parameters:
#   alpha = 0.8662721 
#   beta  = 0.334218 
#   gamma = 0.0001003733 
#   phi   = 0.9369984 

# Initial states:
#   l[0]     b[0]      s[0]    s[-1]     s[-2]    s[-3]
# 6625.332 58.62989 -40.90182 29.35921 -9.978406 21.52102

# sigma^2:  2710.225











