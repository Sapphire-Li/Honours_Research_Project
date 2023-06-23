library(fpp3)
library(tidyverse)
library(dplyr)

# Read data and make some modifications
sp <- read.csv("SP500.csv")
# 2013-2023: 10 years in total

sp <- sp |> 
  filter(SP500 != ".") |>
  mutate(trading_day = row_number(), SP500 = as.numeric(SP500)) |> 
  as_tsibble(index=trading_day, regular=TRUE)


# 分数据方法2
# Split the training set for model estimation (in proportion)
# 1600 days as the training set
# 400 days as the test set
sp_2 <- sp |> 
  filter(trading_day > 519) |> 
  mutate(trading_day = row_number())
train_2 <- sp_2 |> filter(trading_day <= 1600)
test_2 <- sp_2 |> filter(trading_day > 1600)

t <- nrow(train_2)
T <- nrow(test_2)

fit_auto <- train_2 |> 
  model(
    arma = ARIMA(SP500),
    ets = ETS(SP500)
  )


# Model: ARIMA(0,1,2) w/ drift
fit_auto |> 
  select(arma) |> 
  report()
# Coefficients:
#   ma1     ma2  constant
# -0.1736  0.1434    1.4290
# s.e.   0.0246  0.0255    0.7484
# 
# sigma^2 estimated as 954.1

arma_coef <- coef(fit_auto) |> 
  filter(.model == "arma") |> 
  select(term, estimate)
# arma_coef[1,2] # theta_1
# arma_coef[2,2] # theta_2
# arma_coef[3,2] # phi_0



# Model: ETS(M,A,N) 
fit_auto |> 
  select(ets) |> 
  report()

# Smoothing parameters:
#   alpha = 0.8243294 
# 
# Initial states:
#   l[0]
# 2095.452
# 
# sigma^2:  1e-04


ets_coef <- coef(fit_auto) |> 
  filter(.model == "ets") |> 
  select(term, estimate)
# ets_coef[1,2] # alpha
# ets_coef[2,2] # l[0]


# Residuals from 1 to t
ets_resid <- residuals(fit_auto) |> 
  filter(.model == "ets") |> 
  as_tibble() |> 
  select(.resid)

level <- numeric(t+T+1) |> as.numeric()
level[1] <- ets_coef[2,2] # l_0

# level[2] <- level[1]*(1 + ets_coef[1,2]*ets_resid[1,1]) # l_1
# level[3] <- level[2]*(1 + ets_coef[1,2]*ets_resid[2,1]) # l_2

# Level and Trend from initial value (0) to t=1000 (1001 in total)
for (j in 1:t) {
  level[j+1] <- level[j]*(1 + ets_coef[1,2]*ets_resid[j,1])
}


# Residuals from t+1 to T
ets_resid_new <- numeric(T) |> as.numeric()


for(i in 1:T) {
  ets_resid_new[i] <- (test_2[i,2] - level[t+i]) / level[t+i]
  level[t+i+1] <- level[t+i]*(1 + ets_coef[1,2]*ets_resid_new[i])
}


# Conditional Mean for SP500
ets_mean <- numeric(T) |> as.numeric()
for (j in 1:T) {
  ets_mean[j] <- level[[1000+j]]
}


# (Conditional) Variance for SP500 - assume it to be the variance of the error term
ets_var <- glance(fit_auto) |> 
  filter(.model =="ets") |> 
  select(sigma2)
ets_sd <- sqrt(ets_var) |> as.numeric()

# Way too SMALL!!!


ets_auto <- train_2 |> 
  model(
    ets = ETS(SP500)
  )
temp <- generate(ets_auto, times=1000)
ets_sd <- sd(temp$.sim)

ets_pd <- numeric(T) |> as.numeric()
for (j in 1:T) {
  ets_pd[j] <- dnorm(test_2[[j,2]], ets_mean[j], ets_sd)
}

LS_ets <- sum(log(ets_pd)) / T
LS_ets















